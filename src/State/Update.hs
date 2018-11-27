module State.Update (updateState) where

import DataStructure
import EvalAExp (evalAExp)
import qualified Data.Map as Map

updateState :: Var -> AExp -> State -> State -- substituions in State
updateState x aExp (Valid s) = Valid (Map.insert x v s)
  where v = evalAExp aExp (Valid s)
