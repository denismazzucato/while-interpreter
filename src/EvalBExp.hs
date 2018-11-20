module EvalBExp (evalBExp) where

import DataStructure
import EvalAExp

evalBExp :: BExp -> State -> Bool -- Table 1.2
evalBExp (Boolean b) _ = b
evalBExp (ABExp Equal a0 a1) s
  | (evalAExp a0 s) == (evalAExp a1 s) = True
  | (evalAExp a0 s) /= (evalAExp a1 s) = False
evalBExp (ABExp Greater a0 a1) s
  | (evalAExp a0 s) > (evalAExp a1 s) = True
  | (evalAExp a0 s) <= (evalAExp a1 s) = False
evalBExp (ABExp Smaller a0 a1) s
  | (evalAExp a0 s) < (evalAExp a1 s) = True
  | (evalAExp a0 s) >= (evalAExp a1 s) = False
evalBExp (Not b) s
  | (evalBExp b s) == False = True
  | (evalBExp b s) == True = False
evalBExp (BExp And b0 b1) s
  | ((evalBExp b0 s) == True) && ((evalBExp b1 s) == True) = True
  | ((evalBExp b0 s) == False) || ((evalBExp b1 s) == False) = False
evalBExp (BExp Or b0 b1) s
  | ((evalBExp b0 s) == True) || ((evalBExp b1 s) == True) = True
  | ((evalBExp b0 s) == False) && ((evalBExp b1 s) == False) = False
