module State.Lookup (lookupState) where

import DataStructure
import State.State
import qualified Data.Map as Map

lookupState :: Var -> State -> Int -- lookupState is total function
lookupState v (State ((key, value):xs))
  | v == key = value
  | otherwise = lookupState v (State xs)
lookupState _ _ = unknownValue -- base case