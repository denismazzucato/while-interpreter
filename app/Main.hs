module Main where

import Lib
import Semantic
import DataStructure

main :: IO ()
main = someFunc program

-- program = Sequence (Assign "fact" (IntLit 1))
-- $ Sequence (Assign "val" (IntLit 10000))
-- $ Sequence (Assign "cur" (Var "val"))
-- $ Sequence (Assign "mod" (IntLit 1000000007))
-- $ Sequence (While (RBExp Gt (Var "cur") (IntLit 1))
--   $ Sequence (Assign "fact" (AExp Mul (Var "fact") (Var "cur")))
--   $ Sequence (Assign "fact" (AExp Sub (Var "fact") (AExp Mul (AExp Div (Var "fact") (Var "mod")) (Var "mod"))))
--   $ Assign "cur" (AExp Sub (Var "cur") (IntLit 1))
-- ) $ Assign "cur" (IntLit 0)

program = Composition (Assignment "x" (Numeral (5)))
  $ Composition (Assignment "y" (Numeral 1))
  $ (While (Not (ABExp Equal (Variable "x") (Numeral 1)))
    $ Composition (Assignment "y" (AExp Mul (Variable "y") (Variable "x")))
    $ Assignment "x" (AExp Sub (Variable "x") (Numeral 1))
  )