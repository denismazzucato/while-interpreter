module EvalBExp (evalBExp) where

import DataStructure
import EvalAExp

-- while
evalBExp :: BExp -> State -> Bool -- Table 1.2
evalBExp (Boolean b) _ = b
evalBExp (ABExp Equal a0 a1) s = (evalAExp a0 s) == (evalAExp a1 s)
evalBExp (ABExp SmallerThen a0 a1) s = (evalAExp a0 s) <= (evalAExp a1 s)
evalBExp (Not b) s = not (evalBExp b s)
evalBExp (BExp And b0 b1) s = (evalBExp b0 s) && (evalBExp b1 s)

-- while plus
evalBExp (ABExp GreaterThen a0 a1) s = evalBExp (BExp Or notSt eq) s
  where
    notSt = Not (ABExp SmallerThen a0 a1)
    eq = ABExp Equal a0 a1
evalBExp (ABExp Greater a0 a1) s = evalBExp (BExp And gt notEq) s
  where
    gt = ABExp GreaterThen a0 a1
    notEq = Not (ABExp Equal a0 a1)
evalBExp (ABExp Smaller a0 a1) s = evalBExp (BExp And st notEq) s
  where
    st = ABExp SmallerThen a0 a1
    notEq = Not (ABExp Equal a0 a1)
evalBExp (BExp Or b0 b1) s = evalBExp (nor (nor b0 b0) (nor b1 b1)) s
  where
    nor t0 t1 = Not (BExp And b0 b1)
