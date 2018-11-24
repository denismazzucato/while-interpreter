module Lib
  (
    interpret,
    interpretParsed
  ) where

import Semantic
import DataStructure
import State.State
import Parse

interpret :: String -> State
interpret = interpretParsed . parseString

interpretParsed :: Stm -> State
interpretParsed stm = semFunction stm emptyState
