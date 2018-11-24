module DataStructure
  ( State(..),
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
type MapWrapper = Map.Map Var Integer

data State = Valid MapWrapper | Undef
  deriving (Show, Eq)

-- Expressions (Aritmetical and Boolean)

data AExp = -- Table 1.1
  Numeral Integer
  | Variable Var
  | AExp AOp AExp AExp
  deriving (Show, Eq)

data BExp = -- Table 1.2
  Boolean Bool
  | ABExp ABOp AExp AExp -- Boolean Operator for Aritmetical expressions
  | Not BExp
  | BExp BOp BExp BExp -- Boolean Operator for Boolean expressions
  deriving (Show, Eq)

-- Operators

data AOp = Sum | Sub | Mul -- Table 1.1
  deriving (Show, Eq)

data BOp = And | Or -- Table 1.2
  deriving (Show, Eq)

data ABOp = Equal | Greater | Smaller -- Table 1.2
  deriving (Show, Eq)

-- Statements

data Stm =
  Assignment Var AExp
  | Skip
  | Composition Stm Stm
  | If BExp Stm Stm
  | While BExp Stm
  | Repeat Stm BExp
  | For Var AExp AExp Stm
  deriving (Show, Eq)