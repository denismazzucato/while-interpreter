import Lib
import Semantic
import DataStructure
import qualified Data.Map as Map

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests" [testWhileFact, testForLoop])

-- 5!
factParsedProgram = Composition (Assignment "x" (Numeral (5)))
  $ Composition (Assignment "y" (Numeral 1))
  $ (While (Not (ABExp Equal (Variable "x") (Numeral 1)))
    $ Composition (Assignment "y" (AExp Mul (Variable "y") (Variable "x")))
    $ Assignment "x" (AExp Sub (Variable "x") (Numeral 1))
  )

-- For
forLoopParsedProgram = Composition (Assignment "x" (Numeral (1)))
  $ (For "i" (Numeral 0) (Numeral 5)
    $ Assignment "x" (AExp Sum (Variable "x") (Numeral 1))
  )

testWhileFact :: TestTree
testWhileFact = testCase "while factorial 5!"
  (assertEqual "assert label" (Valid (Map.fromList [("x", 1), ("y", 120)])) (interpret factParsedProgram))

testForLoop :: TestTree
testForLoop = testCase "for loop" (assertEqual "label" a b)
  where
    a = Valid (Map.fromList [("i", 5), ("x", 6)])
    b = interpret forLoopParsedProgram
