import Lib
import Semantic
import DataStructure
import qualified Data.Map as Map

main :: IO ()
main = putStrLn . show
  $ interpret program == Valid (Map.fromList [("x", 1), ("y", 120)])

-- 5!
program = Composition (Assignment "x" (Numeral (5)))
  $ Composition (Assignment "y" (Numeral 1))
  $ (While (Not (ABExp Equal (Variable "x") (Numeral 1)))
    $ Composition (Assignment "y" (AExp Mul (Variable "y") (Variable "x")))
    $ Assignment "x" (AExp Sub (Variable "x") (Numeral 1))
  )