module Euler.P12.Solution where

import qualified Math.NumberTheory.Primes.Factorisation as F

triangles :: [Integer]
triangles = triangle 0 0
    where triangle sum index = (sum+index+1) : triangle (sum+index+1) (index+1)
	
answer = head $ filter (\i -> (length $ F.divisors i) > 500) triangles