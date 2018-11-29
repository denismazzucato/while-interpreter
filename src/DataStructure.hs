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
import IntWrapper

-- State

type Var = String
type StateMap = Map.Map Var IntWrapper
data State = State StateMap | Undef deriving (Show, Read, Eq)
-- Undef only for definition of bottom

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