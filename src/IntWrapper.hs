module IntWrapper (IntWrapper(..), intWrapper2Int, int2IntWrapper) where

-- a number can be unknown
data IntWrapper = I Int | Unknown
  deriving (Show, Read, Eq)

int2IntWrapper :: Int -> IntWrapper
int2IntWrapper n = I n

intWrapper2Int :: IntWrapper -> Int
intWrapper2Int (I n) = n
intWrapper2Int _ = unknownValue

unknownValue :: Int
unknownValue = 0