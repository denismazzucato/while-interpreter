module State.Lookup (lookupState) where

import DataStructure
import State.State
import qualified Data.Map as Map

lookupState :: Var -> State -> Integer -- lookupState is total function
lookupState v s = Map.findWithDefault unknownValue v s