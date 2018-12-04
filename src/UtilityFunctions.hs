module UtilityFunctions (cond) where

import DataStructure

cond ::
  (State -> Bool, State -> a, State -> a) ->
  State -> -- s
  a
cond (b, s0, s1) s
  | b s = s0 s
  | otherwise = s1 s