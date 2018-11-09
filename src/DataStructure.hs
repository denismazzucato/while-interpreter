module DataStructure
  ( State(..),
    bottom,
    emptyState,
    undefState,
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

type Var = Char
type MapWrapper = Map.Map Var Integer

data State = Valid MapWrapper | Undef
  deriving (Show, Eq)

bottom :: State -> State
bottom = \s -> Undef

emptyState :: State
emptyState = Valid Map.empty

undefState ::  State
undefState = Undef

-- build state like:
-- State (Map.fromList[('x', 2), ('y'), 3])

-- Undef for function that are undefined in s but have to return a valid State

updateState :: Var -> AExp -> State -> State
updateState x aExp (Valid s) = Valid (Map.insert x v s)
  where v = evalAExp aExp (Valid s)

lookupState :: Var -> State -> Integer
lookupState v (Valid s) = Map.findWithDefault 0 v s

-- Expressions (Aritmetical and Boolean)

data AExp =
  Numeral Integer
  | Variable Var
  | AExp AOp AExp AExp

evalAExp :: AExp -> State -> Integer
evalAExp (Numeral n) _ = n
evalAExp (Variable v) s = lookupState v s
evalAExp (AExp Sum a0 a1) s = (evalAExp a0 s) + (evalAExp a1 s)
evalAExp (AExp Sub a0 a1) s = (evalAExp a0 s) - (evalAExp a1 s)
evalAExp (AExp Mul a0 a1) s = (evalAExp a0 s) * (evalAExp a1 s)

data BExp =
  Boolean Bool
  | ABExp ABOp AExp AExp -- Boolean Operator for Aritmetical expressions
  | Not BExp
  | BExp BOp BExp BExp -- Boolean Operator for Boolean expressions

evalBExp :: BExp -> State -> Bool
evalBExp (Boolean b) _ = b
evalBExp (ABExp Equal a0 a1) s = (evalAExp a0 s) == (evalAExp a1 s)
evalBExp (ABExp Greater a0 a1) s = (evalAExp a0 s) > (evalAExp a1 s)
evalBExp (Not b) s = (evalBExp b s) == False
evalBExp (BExp And b0 b1) s = (evalBExp b0 s) && (evalBExp b1 s)

-- Operators

data AOp = Sum | Sub | Mul

data BOp = And

data ABOp = Equal | Greater
