module Flite.LambdaLift (lambdaLift) where

import Flite.Pretty --temp
import Flite.Syntax
import Flite.Traversals
import Flite.Descend
import Flite.WriterState
import Control.Monad

-- Introduces functions of the form "f^N" where is is a natural
-- number.  Therefore assumes function identifiers do not already
-- contain '^' character.

lambdaLift :: Char -> Prog -> Prog
lambdaLift c prog = concatMap (liftDecl (map funcName prog) c) prog

type Lift a = WriterState Decl Int a

liftDecl :: [Id] -> Char -> Decl -> [Decl]
liftDecl globalIds c (Func f args rhs) = Func f args rhs' : ds
  where
    (_, ds, rhs') = runWS (lift globalIds c f rhs) 0

collectLets :: Binding -> Exp -> Exp
collectLets b (Let bs body) = Let (b:bs) body
collectLets b tm            = Let [b] tm

lift :: [Id] -> Char -> Id -> Exp -> Lift Exp
lift globalIds c f (Let ((var,Lam vs e):rest) body) =
  do i <- get
     set (i+1)
     let f' = f ++ "Lift" ++ c : show i
     let e' = subst (Fun f') var e
     e' <- lift globalIds c f e'
     let ws = filter (`notElem` vs) (freeVarsExcept globalIds e')
     write (Func f' (map Var (ws ++ vs)) e')
     rest' <- lift globalIds c f (Let rest body)
     return $ collectLets (var, App (Fun f') (map Var ws)) rest'
-- TODO are there situations where we will have a partially applied lambda above?
lift globalIds c f (Let [] body) =
  do body' <- lift globalIds c f body
     return (Let [] body')

lift globalIds c f (Lam [] e) = lift globalIds c f e
lift globalIds c f (Lam vs e) =
  do let ws = filter (`notElem` vs) (freeVarsExcept globalIds e)
     i <- get
     set (i+1)
     let f' = f ++ "Lift" ++ c : show i
     e' <- lift globalIds c f e
     write (Func f' (map Var (ws ++ vs)) e')
     return (App (Fun f') (map Var ws))
lift globalIds c f e = descendM (lift globalIds c f) e
