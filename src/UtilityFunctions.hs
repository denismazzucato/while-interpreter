module UtilityFunctions (cond, comp) where

import DataStructure

cond ::
  (State -> Bool, State -> PartialState, State -> PartialState) -> -- s1
  State -> -- s
  PartialState
cond (b, s0, s1) s
  | b s = s0 s
  | otherwise = s1 s

-- comp act like (.) operator => (s0;s1) = s1 . s0 = comp s1 s0
-- the 2nd arg is evaluated first
comp ::
  (State -> PartialState) -> -- s1
  (State -> PartialState) -> -- s0
  State -> PartialState
comp s1 s0 = s1 . destructState . s0
  where destructState (Def s') = s'