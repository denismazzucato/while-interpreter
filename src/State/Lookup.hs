module State.Lookup (
  lookupState
  ) where

import DataStructure
import qualified Data.Map as Map

lookupState :: Var -> State -> Integer
lookupState v (Valid s) = s Map.! v