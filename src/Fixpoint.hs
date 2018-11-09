module Fixpoint
  ( fix
  ) where

import DataStructure

fix :: ((State -> State) -> (State -> State)) -> State -> State
fix f = lub [ fnth f n bottom | n <- [0..] ]

-- nth application of functional F
fnth ::
  ((State -> State) -> (State -> State)) -> -- f
  Integer -> -- n
  (State -> State) ->
  State -> State
fnth f 0 = id
fnth f n = f . (fnth f (n-1))

lub :: [(State -> State)] -> State -> State
lub [] s = bottom s
lub (g:gs) s
  | g s /= Undef = g s
  | otherwise = lub gs s
