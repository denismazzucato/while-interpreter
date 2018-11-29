module UtilityFunctions (cond) where

import DataStructure

cond ::
  (State -> Bool, State -> State, State -> State) ->
  State -> -- s
  State
cond (b, s0, s1) s
  | b s = s0 s
  | otherwise = s1 s