module Fixpoint
  ( fix
  ) where

import DataStructure
import UtilityFunctions
import State.State

fix ::
  ((State -> State) -> State -> State) -> -- f
  State -> State
fix f = lub [ fnth f n bottom | n <- [0..] ] -- Theorem 4.37

-- nth application of functional F
fnth :: -- definition of f^n, Theorem 4.37
  ((State -> State) -> (State -> State)) -> -- f
  Int -> -- n
  (State -> State) ->
  State -> State
fnth f 0 = id
fnth f n = f . (fnth f (n-1))

lub :: [(State -> State)] -> State -> State
-- lub [] s = bottom s -- Fact 4.24
lub (g:gs) s -- Lemma 4.25
  | g s /= Undef = g s -- if exist g (and g s) is unique, also g is the least
  | otherwise = lub gs s
