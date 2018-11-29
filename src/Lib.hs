module Lib
  (
    interpret,
    interpretParsed
  ) where

import Semantic
import DataStructure
import State.State
import Parse

interpret :: String -> String -> State
interpret = interpretParsed . parseString

interpretParsed :: Stm -> String -> State
interpretParsed stm = semFunction stm . read
