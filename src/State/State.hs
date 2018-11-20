module State.State (
  bottom,
  emptyState
  ) where

import DataStructure
import qualified Data.Map as Map

bottom :: State -> State
bottom = \s -> Undef -- Lemma 4.13

emptyState :: State
emptyState = Valid Map.empty