module State.Lookup (
  lookupState
  ) where

import DataStructure
import qualified Data.Map as Map

lookupState :: Var -> State -> Integer
lookupState v (Valid s)
  | Map.member v s = s Map.! v
  | otherwise = error ("Variable not in scope: " ++ v)