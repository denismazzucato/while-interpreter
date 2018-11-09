module Lib
  (
    someFunc,
    interpret
  ) where

import Semantic
import DataStructure

someFunc :: Stm -> IO ()
someFunc = putStr . show . interpret

interpret :: Stm -> State
interpret stm = semFunction stm emptyState
