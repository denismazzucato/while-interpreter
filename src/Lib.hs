module Lib
  (
    interpret,
    interpretParsed
  ) where

import Semantic
import DataStructure
import State.State
import Parse

interpret :: String -> PartialState
interpret = interpretParsed . parseString

interpretParsed :: Stm -> PartialState
interpretParsed stm = semFunction stm emptyState
