module Lib
  (
    interpret,
    interpretParsed
  ) where

import Semantic
import DataStructure
import State.State
import Parse

interpret :: String -> Partial State
interpret = interpretParsed . parseString

interpretParsed :: Stm -> Partial State
interpretParsed stm = semFunction stm emptyState
