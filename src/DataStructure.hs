module DataStructure
  ( State,
    Partial(..),
    Var,
    AExp(..),
    BExp(..),
    AOp(..),
    BOp(..),
    ABOp(..),
    Stm(..)
  ) where

import qualified Data.Map as Map

-- State

type Var = String
type State = Map.Map Var Integer

data Partial a = Def a | Undef
  deriving (Show, Eq)

instance Functor Partial where
  fmap g (Def s) = Def (g s)

instance Applicative Partial where
  pure = Def
  (Def g) <*> mx = fmap g mx

-- Expressions (Aritmetical and Boolean)

data AExp =
  Numeral Integer
  | Variable Var
  | AExp AOp AExp AExp
  deriving (Show, Eq)

data BExp =
  Boolean Bool
  | ABExp ABOp AExp AExp -- Boolean Operator for Aritmetical expressions
  | Not BExp
  | BExp BOp BExp BExp -- Boolean Operator for Boolean expressions
  deriving (Show, Eq)

-- Operators

data AOp = Sum | Sub | Mul
  deriving (Show, Eq)

data BOp = And | Or
  deriving (Show, Eq)

data ABOp = Equal | GreaterThen | SmallerThen | Greater | Smaller
  deriving (Show, Eq)

-- Statements

data Stm =
  Assignment Var AExp
  | Skip
  | Composition Stm Stm
  | If BExp Stm Stm
  | While BExp Stm
  | Repeat Stm BExp
  | RepeatSS Stm BExp -- Syntactic Sugar version
  | For Var AExp AExp Stm
  deriving (Show, Eq)