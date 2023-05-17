module Flite.Syntax where
type Prog = [Decl]

data Decl = Func { funcName :: Id
                 , funcArgs :: [Pat]
                 , funcRhs  :: Exp }
          | Other String

type Id = String

data AltsTab = AFuns   [Id]          -- Compile to one template base address
             | AInline [([Id], Exp)] -- Compile with inlined single atoms as
  deriving Eq                        -- each alt

data Exp = App Exp [Exp]
         | Case Exp [Alt]
         | Let [Binding] Exp
         | Var Id
         | Con Id
         | Fun Id
         | Int Int
         | Wld -- Wildcard '_'

           -- The following may be introduced by various transformations,
           -- but not by the parser.
         | Bottom
         | Alts AltsTab Int
         | Ctr Id Int Int
         | Lam [Id] Exp

           -- For speculative evaluation of primitive redexes.
         | PrimApp Id [Exp]
         | Prim Id
  deriving Eq

type Pat = Exp

type Alt = (Pat, Exp)

type Binding = (Id, Exp)

type App = [Exp]

-- Primitive functions

isPrimId :: Id -> Bool
isPrimId p = isBinaryPrim p || isUnaryPrim p || isTernaryPrim p

isBinaryPrim :: Id -> Bool
isBinaryPrim "(+)"  = True
isBinaryPrim "(-)"  = True
isBinaryPrim "(==)" = True
isBinaryPrim "(/=)" = True
isBinaryPrim "(<=)" = True
isBinaryPrim "(.&.)"  = True
isBinaryPrim "ld32"  = True
isBinaryPrim _      = False

isUnaryPrim :: Id -> Bool
isUnaryPrim "(!)"  = True
isUnaryPrim "emit" = True
isUnaryPrim "emitInt" = True
isUnaryPrim _ = False

isTernaryPrim :: Id -> Bool
isTernaryPrim "st32" = True
isTernaryPrim ('s':'w':'a':'p':':':f) = isTernaryPrim f
isTernaryPrim _ = False

isPredexId :: Id -> Bool
isPredexId = isBinaryPrim
