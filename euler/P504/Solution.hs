module Euler.P504.Solution where

import Math.NumberTheory.Powers.Squares

-- Points on the line (a,0) -> (0,b)
diagonal :: Int -> Int -> Int
diagonal a b = gcd a b + 1

-- origin (0,0) counted twice
boundary a b = a + b + diagonal a b - 1

{- Pick's Theorem:
P(a,b) = A + B/2 + 1
where A = a*b/2  -- area
      B = a+b+diagonal(a,b)-3 -- boundary

-}
-- Points strictly inside triangle [(0,0),(a,0),(0,b)] (not including boundary)
containedT :: Int -> Int -> Int
containedT a b = (a*b - boundary a b) `div` 2 + 1

containedQ :: (Int, Int, Int, Int) -> Int
containedQ (a,b,c,d) = containedT a b + containedT b c + containedT c d + containedT d a + a + b + c + d - 3

countSquares m =
    let as = [1..m]
        bs = [1..m]
        cs = [1..m]
        ds = [1..m]
        combos = [ (a,b,c,d) | a<-as, b<-bs, c<-cs, d<-ds ]
    in length $ filter isSquare $ map containedQ combos

main = print $ countSquares 100