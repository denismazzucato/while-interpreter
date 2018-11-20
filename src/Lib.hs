module Lib
  (
    someFunc,
    interpret
  ) where

import Semantic
import DataStructure
import State.State
import Parse

someFunc :: String -> IO ()
someFunc = putStr . show . interpret . parseString

interpret :: Stm -> State
interpret stm = semFunction stm emptyState
