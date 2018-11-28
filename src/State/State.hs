module State.State (bottom, emptyState) where

import DataStructure
import qualified Data.Map as Map

bottom :: State -> Partial State
bottom = \s -> Undef -- Lemma 4.13

emptyState :: State
emptyState = Map.empty