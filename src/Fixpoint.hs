module Fixpoint
  ( fix
  ) where

import DataStructure

fix :: ((State -> State) -> (State -> State)) -> State -> State
fix f s = head [ app n | n <- [0..], app n /= Undef]
  where
    app = \n -> fnth f n bottom s

-- nth application of functional F
fnth ::
  ((State -> State) -> (State -> State)) -> -- f
  Integer -> -- n
  (State -> State) ->
  State -> State
fnth f 0 = id
fnth f n = f . (fnth f (n-1))
