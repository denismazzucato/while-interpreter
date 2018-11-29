module IntWrapper (IntWrapper(..), intWrapper2Int, int2IntWrapper) where

-- a number can be unknown
data IntWrapper = I Integer | Unknown
  deriving (Show, Read, Eq)

int2IntWrapper :: Integer -> IntWrapper
int2IntWrapper n = I n

intWrapper2Int :: IntWrapper -> Integer
intWrapper2Int (I n) = n
intWrapper2Int _ = unknownValue

unknownValue :: Integer
unknownValue = 0