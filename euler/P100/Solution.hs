module Euler.P100.Solution where

import Data.List (find) 

-- Algebraically-derived generating function for Diophantine pairs (Pell's equation)
arrangements n b = 
    let n' = 3 * n + 4 * b - 3
        b' = 2 * n + 3 * b - 2
    in (n', b'):arrangements n' b'

answer =  find ((>10^12).fst) (arrangements 1 1)