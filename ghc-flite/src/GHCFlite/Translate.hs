module GHCFlite.Translate where

import qualified Flite.Syntax as F
import Flite.LambdaLift (lambdaLift)

import CoreSyn (Alt, AltCon (..), Bind (..), CoreProgram, CoreBind, Expr (..), isValArg, collectBinders, collectArgs, isTyCoArg)
import Var (Var, Id, idDetails, varName, isTyCoVar, isNonCoVarId)
import Literal (Literal (..))
import Name (NamedThing, getOccString, occNameString)
import TyCoRep (Type (..))
import IdInfo (IdDetails (..))
import PrimOp (PrimOp (..), primOpOcc)
import Class (classKey)
import Outputable (ppr, showSDocUnsafe)
import FastString (unpackFS)
import BasicTypes (FunctionOrData(..))
import CoreSubst

import Data.Maybe (catMaybes, fromMaybe)
import Data.List (isPrefixOf, intersperse, nub)

translate :: CoreProgram -> F.Prog
translate = lambdaLift 'A' .
            concat . map translateBind

translateBind :: CoreBind -> [F.Decl]
translateBind (NonRec var expr)
  | "$tr" `isPrefixOf` getName' var = []
  | "$tc" `isPrefixOf` getName' var = []
  | otherwise =
      let (args, body) = collectBinders expr
          args' = filter isTermId args
          fbody = translateExpr body
      in [F.Func { F.funcName = getName $ var
                 , F.funcArgs = map (F.Var . getName) args'
                 , F.funcRhs  = fbody }]
translateBind (Rec bs) = concat $ map (\(v,expr) -> translateBind $ NonRec v expr) bs

translateExpr :: Expr Var -> F.Exp

translateExpr (Var id) = toRef id
translateExpr (Lit lit) = translateLit lit

translateExpr expr@(App _ _)
  = let (f, args) = collectArgs expr
        f' = translateExpr f
        args' = map translateExpr (filter isTerm args)
    in go f' args'
  where go (F.Var id) [x]
          | id `elem` ignoredUnaryFns = x
        go (F.Fun id) [x]
          | id `elem` ignoredUnaryFns = x
        go f args = F.App f args

translateExpr expr@(Lam _ _)
  = let (args, body) = collectBinders expr
        args' = map getName $ filter isTermId args
        body' = translateExpr body
    in F.Lam args' body'

translateExpr (Let (NonRec var val) body)
  -- | isTermId var
      = let var' = getName var
            val' = translateExpr val
            body' = translateExpr body
        in F.Let [(var',val')] body'
  -- | otherwise = translateExpr body

translateExpr (Let (Rec bs) body)
  = let bs' = bs -- filter (isTermId . fst) bs
        vars' = map (getName . fst) bs'
        vals' = map (translateExpr . snd) bs'
        body' = translateExpr body
    in F.Let (zip vars' vals') body'

-- Handle unboxing integer cases
translateExpr (Case scr _ ty [(DataAlt dataCon, [num], expr)])
  | getName dataCon == "I#" = let sub = extendIdSubst emptySubst num scr
                              in translateExpr $ substExpr (ppr expr) sub expr
-- Handle single default cases introduced by optimisations
translateExpr (Case scr _ ty [(DEFAULT, [], expr)])
  = translateExpr expr

translateExpr (Case expr id ty alts)
  = let scr = translateExpr expr
        sub = extendIdSubst emptySubst id expr
        alts' = map (translateAlt . updateAlt (ppr id) sub) alts
    in F.Case scr alts'
  where updateAlt id sub (con, args, scope) = (con, args, substExpr id sub scope)
   -- Maybe we should see if id is a free variable in any alts.
   -- If so, let bind it!

-- Type expression that we should avoid translating
translateExpr (Cast expr _coercion) = error "Cannot translate cast"
translateExpr (Tick _id _expr)      = error "Cannot translate tick"
translateExpr (Type ty)             = error "Cannot translate type"
translateExpr (Coercion _coercion)  = error "Cannot translate coersion"

translateAlt :: Alt Var -> F.Alt
translateAlt (DataAlt dataCon, vars, expr)
  = let rhs = translateExpr expr
    in (F.App (F.Con $ getName dataCon) (map (F.Var . getName) vars), rhs)
translateAlt (LitAlt literal, _vars, expr)
  = let rhs = translateExpr expr
    in (translateLit literal, rhs)
translateAlt (DEFAULT, [var], expr)
  = let rhs = translateExpr expr
    in (F.Var (getName var), rhs)
translateAlt (DEFAULT, [], expr)
  = let rhs = translateExpr expr
    in (F.Wld, rhs)

-- TODO We should probably return F.Exp here
-- Really important that primitives aren't marked as Var since they will be included in the environment during lambda-lifting
translatePrimName :: String -> String
translatePrimName "+"     = "(+)"
translatePrimName "-"     = "(-)"
translatePrimName "=="    = "(==)"
translatePrimName "/="    = "(/=)"
translatePrimName "<="    = "(<=)"
translatePrimName "plusInteger"  = "(+)"
translatePrimName "minusInteger" = "(-)"
translatePrimName "eqInteger#"    = "(==)"
translatePrimName "neInteger#"    = "(/=)"
translatePrimName "leInteger#"    = "(<=)"
translatePrimName "print" = "emitInt"
translatePrimName ":" = "Cons"
translatePrimName "[]" = "Nil"
translatePrimName "(,)" = "MkTup"
translatePrimName "(#,#)" = "MkTup"
translatePrimName "C:Ord" = "COrd"
translatePrimName "C:Eq" = "CEq"
translatePrimName n       = n

isPrimName :: String -> Bool
isPrimName "(+)"  = True
isPrimName "(-)"  = True
isPrimName "(==)" = True
isPrimName "(/=)" = True
isPrimName "(<=)" = True
isPrimName _      = False

translatePrimOp :: PrimOp -> F.Id
translatePrimOp IntAddOp = "(+)"
translatePrimOp IntSubOp = "(-)"
translatePrimOp IntEqOp  = "(==)"
translatePrimOp IntNeOp  = "(/=)"
translatePrimOp IntLeOp  = "(<=)"
--translatePrimOp ?? = "emit"
--translatePrimOp ?? = "emitInt"
translatePrimOp TagToEnumOp = "tagToEnum" -- TODO should handle the application
                                          -- of this and try to preserve
                                          -- original constructors...
                                          -- Just now we only offer this clause to
                                          -- prevent errors in codegen for -O2
translatePrimOp p           = error $ "Primitive operation not supported: " ++ (occNameString $ primOpOcc p)

translateLit :: Literal -> F.Exp
translateLit (LitNumber _ n _)
  | n >= 0 = F.Int . fromInteger $ n
  | otherwise = F.App (F.Fun "(-)") [F.Int 0, F.Int $ fromInteger $ negate n]
translateLit (MachLabel  name _ IsFunction) = F.Fun (unpackFS name)
translateLit (MachLabel  name _ IsData) = F.Con (unpackFS name)
translateLit (MachChar c) = F.Var $ show c
translateLit (MachStr bs) = F.Var $ show bs
translateLit lit = error $ "unknown literal: " ++ showSDocUnsafe (ppr lit)

isTermId :: Id -> Bool
isTermId id
  | isNonCoVarId id =
      case idDetails id of
        DFunId _      -> False
        FCallId _     -> False
        TickBoxOpId _ -> False
        CoVarId       -> False
        ClassOpId cls -> False
        --JoinId _      -> False
        _             -> not $ (isPrefixOf "$" (getName' id) ) -- || (getName id) == "lvl") -- LVL thing is major hack
  | otherwise = False

isFun :: Id -> Bool
isFun id = case idDetails id of
             DFunId f  -> True
             _         -> False

isTerm :: Expr Var -> Bool
isTerm (Var id) = isTermId id
isTerm e = not $ isTyCoArg e

getName' :: NamedThing a => a -> String
getName' = translatePrimName . getOccString

getName :: NamedThing a => a -> String
getName = map sanitise . getName'
  where sanitise '$' = 's'
        sanitise '_' = 'u'
        sanitise '\'' = 'p'
        sanitise c = c

toRef :: Id -> F.Exp
toRef id
  = case idDetails id of
      (PrimOpId primOp) -> F.Fun . translatePrimOp $ primOp
      (ClassOpId cls)   -> F.Fun . getName $ id
      VanillaId         -> let n = getName id
                           in if (isPrimName n) then F.Fun n else F.Var n
      DataConWorkId _   -> F.Con . getName $ id
      DataConWrapId _   -> F.Con . getName $ id
      DFunId f          -> F.Fun . getName $ id
      JoinId arity      -> F.Var . getName $ id
      idType            -> error $ "Unhandled id type: " ++ showSDocUnsafe (ppr idType)

ignoredUnaryFns = ["I#", "C#", "fromInteger", "toInteger"]
