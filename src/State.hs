module State (
  bottom,
  emptyState,
  updateState,
  lookupState
  ) where

import DataStructure
import EvalAExp (evalAExp)
import qualified Data.Map as Map

bottom :: State -> State
bottom = \s -> Undef -- Lemma 4.13

emptyState :: State
emptyState = Valid Map.empty

updateState :: Var -> AExp -> State -> State -- substituions in State
updateState x aExp (Valid s) = Valid (Map.insert x v s)
  where v = evalAExp aExp (Valid s)

lookupState :: Var -> State -> Integer
lookupState v (Valid s) = s Map.! v