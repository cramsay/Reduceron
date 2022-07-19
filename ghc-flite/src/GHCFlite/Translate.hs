module GHCFlite.Translate where

import CoreSyn (Alt, AltCon (..), Bind (..), CoreProgram, CoreBind, Expr (..), isValArg, collectBinders, collectArgs, isTyCoArg)
import Var (Var, Id, idDetails, varName, isTyCoVar, isNonCoVarId)
import Literal (Literal (LitNumber))
import Name (NamedThing, getOccString, occNameString)
import TyCoRep (Type (..))
import IdInfo (IdDetails (..))
import PrimOp (PrimOp (..), primOpOcc)
import Class (classKey)
import Outputable (ppr, showSDocUnsafe)

import Data.Maybe (catMaybes, fromMaybe)
import Data.List (isPrefixOf, intersperse, nub)
import Control.Monad.State

type FProg = [FDecl]

data FDecl = Func { funcName :: FId
                  , funcArgs :: [FPat]
                  , funcRhs  :: FExp }
  deriving Eq

type FId = String

data FExp = FApp FExp [FExp]
         | FCase FExp [FAlt]
         | FLet [FBinding] FExp
         | FVar FId
         | Con FId
         | Fun FId
         | Int Int
  deriving Eq

type FPat = FExp
type FBinding = (FId, FExp)
type FAlt = (FPat, FExp)

-- We need to lambda lift definitions to make valid f-lite source We translate
-- relative to a state of definitions, so we can add new definitions for
-- lambda-lifted expressions. Let's do it via a state monad.

-- Also, let's not depend on flite internals. Just make our own ADT for the language here and pretty print it.

translate :: CoreProgram -> FProg
translate = flip execState [] . mapM translateBind

translateBind :: CoreBind -> State [FDecl] ()
translateBind (NonRec var expr)
  | not (isModName var) =
      do let (args, body) = collectBinders expr
         let args' = filter isTermId args
         fbody <- translateExpr body
         let f = Func { funcName = getName $ var
                      , funcArgs = map (FVar . getName) args'
                      , funcRhs  = fbody }
         modify (f:)
  | otherwise = return ()
translateBind (Rec bs)     = mapM_ translateBind $
                               map (\(v,expr) -> NonRec v expr) bs

translateExpr :: Expr Var -> State [FDecl] FExp

translateExpr (Var id)
  = return $ case idDetails id of
               (PrimOpId primOp) -> Fun . translatePrimOp $ primOp
               (ClassOpId cls)   -> Fun . getName $ id
               VanillaId         -> FVar . getName $ id
               DataConWorkId _   -> Con . getName $ id
               DataConWrapId _   -> Con . getName $ id
               DFunId f          -> Fun . getName $ id
               idType            -> error $ "Unhandled id type: " ++ showSDocUnsafe (ppr idType)
translateExpr (Lit lit) = return $ translateLit lit

translateExpr (App (Var id) i)
  | getName id == "I#" = translateExpr i

translateExpr expr@(App _ _)
  = do let (f, args) = collectArgs expr
       f' <- translateExpr f
       args' <- mapM translateExpr (filter isTerm args)
       return $ FApp (markFun f') args'

-- TO DO Lambda-lifting
translateExpr expr@(Lam _ _)
  = do let (args, body) = collectBinders expr
       let args' = map getName $ filter isTermId args
       body' <- translateExpr body
       let env = filter (`notElem` args') (freeVars body')
       decls <- get
       let fname = "lamlift_" ++ show (length decls)
       modify (Func fname (map FVar (env ++ args')) body' :)
       return (FApp (Fun fname) (map FVar env))

-- TODO if this let is pattern matching, we should lift to a new top-level fn
translateExpr (Let (NonRec var val) body)
  = do let var' = getName var
       val' <- translateExpr val
       body' <- translateExpr body
       return $ FLet [(var',val')] body'
translateExpr (Let (Rec bs) body)
  = translateExpr $ foldl (\scope -> \(var,val) -> Let (NonRec var val) scope) body bs
translateExpr (Case expr id ty alts)
  = do scr <- translateExpr expr
       alts' <- mapM translateAlt alts
       return $ FCase scr alts'

-- Type expression that we should avoid translating
translateExpr (Cast expr _coercion) = error "Cannot translate cast"
translateExpr (Tick _id _expr)      = error "Cannot translate tick"
translateExpr (Type ty)             = error "Cannot translate type"
translateExpr (Coercion _coercion)  = error "Cannot translate coersion"

translateAlt :: Alt Var -> State [FDecl] FAlt
translateAlt (DataAlt dataCon, vars, expr)
  = do rhs <- translateExpr expr
       return (Con $ getName dataCon, rhs)
translateAlt (LitAlt _literal, _vars, _expr)  = error "LitAlt encountered in translateAlt"

isPrimName :: String -> Bool
isPrimName "+"     = True
isPrimName "-"     = True
isPrimName "=="    = True
isPrimName "/="    = True
isPrimName "<="    = True
isPrimName "print" = True
isPrimName _       = False

translatePrimName :: String -> String
translatePrimName "+"     = "(+)"
translatePrimName "-"     = "(-)"
translatePrimName "=="    = "(==)"
translatePrimName "/="    = "(/=)"
translatePrimName "<="    = "(<=)"
translatePrimName "print" = "emitInt"
translatePrimName n       = n

translatePrimOp :: PrimOp -> FId
translatePrimOp IntAddOp = "(+)"
translatePrimOp IntSubOp = "(-)"
translatePrimOp IntEqOp  = "(==)"
translatePrimOp IntNeOp  = "(/=)"
translatePrimOp IntLeOp  = "(<=)"
--translatePrimOp ?? = "emit"
--translatePrimOp ?? = "emitInt"
translatePrimOp p           = error $ "Primitive operation not supported: " ++ (occNameString $ primOpOcc p)

translateLit :: Literal -> FExp
translateLit (LitNumber _ n _) = Int . fromInteger $ n
translateLit lit = error "unknown literal"

isTermId :: Id -> Bool
isTermId id = case idDetails id of
                DFunId _      -> False
                FCallId _     -> False
                TickBoxOpId _ -> False
                CoVarId       -> False
                _             -> True

isTerm :: Expr Var -> Bool
isTerm (Var id) = isTermId id
isTerm e = not $ isTyCoArg e

isModName :: Var -> Bool
isModName = isPrefixOf "$trModule" . getName

markFun :: FExp -> FExp
markFun (FVar id) = Fun id
markFun e = e

getName :: NamedThing a => a -> String
getName = translatePrimName . getOccString

patVars :: FPat -> [FId]
patVars (FApp e es) = concatMap patVars (e:es)
patVars (FVar v) = [v]
patVars p = []

freeVarsExcept :: [FId] -> FExp -> [FId]
freeVarsExcept vs e = nub (freeVarsExcept' vs e)

freeVarsExcept' :: [FId] -> FExp -> [FId]
freeVarsExcept' vs e = fv vs e
  where
    fv vs (FCase e as) =
      fv vs e ++ concat [fv (patVars p ++ vs) e | (p, e) <- as]
    fv vs (FLet bs e) = let ws = map fst bs ++ vs
                       in  fv ws e ++ concatMap (fv ws . snd) bs
    fv vs (FVar w) = [w | w `notElem` vs]
    fv vs (FApp e es) = concat $ map (fv vs) (e:es)
    fv vs _ = []

freeVars :: FExp -> [FId]
freeVars e = nub (freeVarsExcept' [] e)

-- Pretty Printing

consperse :: [a] -> [[a]] -> [a]
consperse x xs = concat (intersperse x xs)

pretty :: FProg -> String
pretty p = "{\n" ++ concatMap show p ++ "}"

instance Show FDecl where
  show (Func name args rhs) = name ++ " "
                           ++ consperse " " (map showArg args)
                           ++ " = "
                           ++ show rhs ++ ";\n"

instance Show FExp where
  show (FApp e es) = consperse " " (showArg e : map showArg es)
  show (FCase e as) = "case " ++ show e ++ " of " ++ showBlock showAlt as
  show (FLet bs e) = "let " ++ showBlock showBind bs ++ " in " ++ show e
  show (FVar v) = v
  show (Fun f) = f
  show (Con c) = c
  show (Int i) = show i

showArg :: FExp -> String
showArg (FApp e []) = showArg e
showArg (FApp e es) = "(" ++ show (FApp e es) ++ ")"
showArg e = show e

showBlock :: (a -> String) -> [a] -> String
showBlock f as = "{ " ++ consperse "; " (map f as) ++ " }"

showAlt :: FAlt -> String
showAlt (p, e) = show p ++ " -> " ++ show e

showBind :: FBinding -> String
showBind (v, e) = v ++ " = " ++ show e
