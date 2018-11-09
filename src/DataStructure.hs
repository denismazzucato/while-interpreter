module DataStructure
  ( State(..),
    bottom,
    emptyState,
    Var(..),
    updateState,
    lookupState,
    AExp(..),
    evalAExp,
    BExp(..),
    evalBExp,
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

bottom :: State -> State
bottom = \s -> Undef -- Lemma 4.13

emptyState :: State
emptyState = Valid Map.empty

-- build state like:
-- State (Map.fromList[('x', 2), ('y'), 3])

-- Undef for function that are undefined in s but have to return a valid State

updateState :: Var -> AExp -> State -> State -- substituions in State
updateState x aExp (Valid s) = Valid (Map.insert x v s)
  where v = evalAExp aExp (Valid s)

lookupState :: Var -> State -> Integer
lookupState v (Valid s) = s Map.! v

-- Expressions (Aritmetical and Boolean)

data AExp = -- Table 1.1
  Numeral Integer
  | Variable Var
  | AExp AOp AExp AExp
  deriving (Show)

evalAExp :: AExp -> State -> Integer -- Table 1.1
evalAExp (Numeral n) _ = n
evalAExp (Variable v) s = lookupState v s
evalAExp (AExp Sum a0 a1) s = (evalAExp a0 s) + (evalAExp a1 s)
evalAExp (AExp Sub a0 a1) s = (evalAExp a0 s) - (evalAExp a1 s)
evalAExp (AExp Mul a0 a1) s = (evalAExp a0 s) * (evalAExp a1 s)

data BExp = -- Table 1.2
  Boolean Bool
  | ABExp ABOp AExp AExp -- Boolean Operator for Aritmetical expressions
  | Not BExp
  | BExp BOp BExp BExp -- Boolean Operator for Boolean expressions
  deriving (Show)

evalBExp :: BExp -> State -> Bool -- Table 1.2
evalBExp (Boolean b) _ = b
evalBExp (ABExp Equal a0 a1) s
  | (evalAExp a0 s) == (evalAExp a1 s) = True
  | (evalAExp a0 s) /= (evalAExp a1 s) = False
evalBExp (ABExp Greater a0 a1) s
  | (evalAExp a0 s) > (evalAExp a1 s) = True
  | (evalAExp a0 s) <= (evalAExp a1 s) = False
evalBExp (ABExp Smaller a0 a1) s
  | (evalAExp a0 s) < (evalAExp a1 s) = True
  | (evalAExp a0 s) >= (evalAExp a1 s) = False
evalBExp (Not b) s
  | (evalBExp b s) == False = True
  | (evalBExp b s) == True = False
evalBExp (BExp And b0 b1) s
  | ((evalBExp b0 s) == True) && ((evalBExp b1 s) == True) = True
  | ((evalBExp b0 s) == False) || ((evalBExp b1 s) == False) = False
evalBExp (BExp Or b0 b1) s
  | ((evalBExp b0 s) == True) || ((evalBExp b1 s) == True) = True
  | ((evalBExp b0 s) == False) && ((evalBExp b1 s) == False) = False

-- Operators

data AOp = Sum | Sub | Mul -- Table 1.1
  deriving (Show)

data BOp = And | Or -- Table 1.2
  deriving (Show)

data ABOp = Equal | Greater | Smaller -- Table 1.2
  deriving (Show)
