module Euler.P53.Solution where

factorial n = product [1..n]

choose n r = (factorial n) `div` ((factorial r) * factorial (n-r))

answer = length $ filter (> 10^6) $ map (uncurry choose) [(n,r) | n <- [1..100], r <- [1..n]]