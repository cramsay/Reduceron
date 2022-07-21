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
translate = lambdaLift 'A' . concat . map translateBind

translateBind :: CoreBind -> [F.Decl]
translateBind (NonRec var expr)
  | "strModule" `isPrefixOf` getName var = []
  | otherwise =
      let (args, body) = collectBinders expr
          args' = filter isTermId args
          fbody = translateExpr body
      in [F.Func { F.funcName = getName $ var
                 , F.funcArgs = map (F.Var . getName) args'
                 , F.funcRhs  = fbody }]
translateBind (Rec bs) = concat $ map (\(v,expr) -> translateBind $ NonRec v expr) bs

translateExpr :: Expr Var -> F.Exp

translateExpr (Var id)
  = case idDetails id of
      (PrimOpId primOp) -> F.Fun . translatePrimOp $ primOp
      (ClassOpId cls)   -> F.Fun . getName $ id
      VanillaId         -> F.Var . getName $ id
      DataConWorkId _   -> F.Con . getName $ id
      DataConWrapId _   -> F.Con . getName $ id
      DFunId f          -> F.Fun . getName $ id
      JoinId arity      -> F.Var . getName $ id
      idType            -> error $ "Unhandled id type: " ++ showSDocUnsafe (ppr idType)
translateExpr (Lit lit) = translateLit lit

translateExpr (App (Var id) i)
  | getName id == "I#" = translateExpr i
  | getName id == "C#" = translateExpr i
  | getName id == "fromInteger" = translateExpr i

translateExpr expr@(App _ _)
  = let (f, args) = collectArgs expr
        f' = translateExpr f
        args' = map translateExpr (filter isTerm args)
    in F.App f' args'

translateExpr expr@(Lam _ _)
  = let (args, body) = collectBinders expr
        args' = map getName $ filter isTermId args
        body' = translateExpr body
    in F.Lam args' body'

translateExpr (Let (NonRec var val) body)
  = let var' = getName var
        val' = translateExpr val
        body' = translateExpr body
    in F.Let [(var',val')] body'
translateExpr (Let (Rec bs) body)
  = let vars' = map (getName . fst) bs
        vals' = map (translateExpr . snd) bs
        body' = translateExpr body
    in F.Let (zip vars' vals') body'

-- Handle unboxing integer cases
translateExpr (Case scr _ ty [(DataAlt dataCon, [num], expr)])
  | getName dataCon == "I#" = let sub = extendIdSubst emptySubst num scr
                              in translateExpr $ substExpr (ppr expr) sub expr
                               -- F.Let [(getName num,F.Var $ getName id)] (translateExpr expr)
-- Handle single default cases introduced by optimisations
translateExpr (Case scr _ ty [(DEFAULT, [], expr)])
  = translateExpr expr

translateExpr (Case expr id ty alts)
  = let scr = translateExpr expr
        alts' = map translateAlt alts
    in F.Case scr alts'

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

isPrimName :: String -> Bool
isPrimName "+"     = True
isPrimName "-"     = True
isPrimName "=="    = True
isPrimName "/="    = True
isPrimName "<="    = True
isPrimName "print" = True
isPrimName _       = False

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
translatePrimName n       = n

translatePrimOp :: PrimOp -> F.Id
translatePrimOp IntAddOp = "(+)"
translatePrimOp IntSubOp = "(-)"
translatePrimOp IntEqOp  = "(==)"
translatePrimOp IntNeOp  = "(/=)"
translatePrimOp IntLeOp  = "(<=)"
--translatePrimOp ?? = "emit"
--translatePrimOp ?? = "emitInt"
translatePrimOp TagToEnumOp = "tte"
translatePrimOp p           = error $ "Primitive operation not supported: " ++ (occNameString $ primOpOcc p)

translateLit :: Literal -> F.Exp
translateLit (LitNumber _ n _) = F.Int . fromInteger $ n
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
        _             -> not $ isPrefixOf "$" (getName' id)
  | otherwise = False

isTerm :: Expr Var -> Bool
isTerm (Var id) = isTermId id
isTerm e = not $ isTyCoArg e

getName' :: NamedThing a => a -> String
getName' = translatePrimName . getOccString

getName :: NamedThing a => a -> String
getName = map sanitise . getName'
  where sanitise '$' = 's'
        sanitise c = c
