module DataStructure
  ( State(..),
    Var(..),
    AExp(..),
    BExp(..),
    AOp(..),
    BOp(..),
    ABOp(..)
  ) where

import qualified Data.Map as Map

-- State

type Var = String
type MapWrapper = Map.Map Var Integer

data State = Valid MapWrapper | Undef
  deriving (Show, Eq)

-- Expressions (Aritmetical and Boolean)

data AExp = -- Table 1.1
  Numeral Integer
  | Variable Var
  | AExp AOp AExp AExp
  deriving (Show)

data BExp = -- Table 1.2
  Boolean Bool
  | ABExp ABOp AExp AExp -- Boolean Operator for Aritmetical expressions
  | Not BExp
  | BExp BOp BExp BExp -- Boolean Operator for Boolean expressions
  deriving (Show)

-- Operators

data AOp = Sum | Sub | Mul -- Table 1.1
  deriving (Show)

data BOp = And | Or -- Table 1.2
  deriving (Show)

data ABOp = Equal | Greater | Smaller -- Table 1.2
  deriving (Show)
