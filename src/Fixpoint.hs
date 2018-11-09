module Fixpoint
  ( fix
  ) where

import DataStructure

fix :: ((State -> State) -> (State -> State)) -> State -> State
fix f s = head [ app n | n <- [0..], app n /= undefState]
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

-- -- Corpo del while S_ds[[S]]s
-- body :: State -> State
-- body (Valid (x, y, z)) = Valid (x-1, y*x, 0)
-- body Undef = Undef

-- condizione (Valid (x, y, z)) = x /= 1
-- condizione Undef = False

-- f :: (State -> State) -> State -> State
-- f g s
--   | condizione s == True  = (g . body) s
--   | condizione s == False = s
-- f g Undef = Undef

-- -- build_functional cond bod g | cond s

-- -- nth applicazione di F
-- fnth :: (State -> State) -> Integer -> State -> State
-- fnth prec 0 s = prec s
-- fnth prec n s = f fn'th s
--   where
--     fn'th = fnth prec (n-1)

-- -- fixpoint_f :: State -> State
-- fixpoint_f s = head [ app n | n <- [0..], app n /= Undef]
--   where
--     app = \n -> fnth (\s -> Undef) n s