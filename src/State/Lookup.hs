module State.Lookup (lookupState) where

import DataStructure
import IntWrapper
import State.State
import qualified Data.Map as Map

lookupState :: Var -> State -> IntWrapper -- lookupState is total function
lookupState v (State ((key, value):xs))
  | v == key = value
  | otherwise = lookupState v (State xs)
lookupState _ _ = Unknown -- base case