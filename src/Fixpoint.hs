module Fixpoint
  ( fix
  ) where

import DataStructure

fix :: ((State -> State) -> (State -> State)) -> State -> State
fix f = lub [ fnth f n bottom | n <- [0..] ] -- Theorem 4.37

-- nth application of functional F
fnth :: -- definition of f^n, Theorem 4.37
  ((State -> State) -> (State -> State)) -> -- f
  Integer -> -- n
  (State -> State) ->
  State -> State
fnth f 0 = id
fnth f n = f . (fnth f (n-1))

lub :: [(State -> State)] -> State -> State
lub [] s = bottom s -- Fact 4.24
lub (g:gs) s -- Lemma 4.25
  | g s /= Undef = g s
  | otherwise = lub gs s
