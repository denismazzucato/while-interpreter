module State.State (bottom, emptyState, unknownValue) where

import DataStructure
import qualified Data.Map as Map

bottom :: State -> State
bottom = \s -> Undef -- Lemma 4.13
-- Undef have to be inside State because bottom
-- is used in place that required function State to State

unknownValue :: Int
unknownValue = (minBound + maxBound) `div` 2

emptyState :: State
emptyState = State []