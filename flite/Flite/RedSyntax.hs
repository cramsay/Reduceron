module Flite.RedSyntax where

type Id = Int

type Arity = Int

type Index = Int

type Shared = Bool

data Atom =
    INT Int
  | ARG Shared Int
  | VAR Shared Int
  | REG Shared Int
  | CON Arity Index
  | FUN Bool Arity Id
  | PRI Arity String
  deriving (Show, Read)

type Normal = Bool

type RegId = Int

data App = APP Normal [Atom] | CASE LUT [Atom] | PRIM RegId [Atom]
  deriving (Show, Read)

data LUT = LOffset Int
         | LInline [(Int, Atom)]
  deriving (Show, Read)


type Template = (String, Int, [LUT], [Atom], [App])

type Prog = [Template]
