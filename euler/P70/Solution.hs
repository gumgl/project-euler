module Euler.P70.Solution where

import Data.List
import Data.Numbers.Primes
import Data.Digits
import Data.Function

groupCount :: Eq a => [a] -> [(a, Int)]
groupCount = map (\(x:xs) -> (x,1+(length xs))) . group

primeFactorsCount :: Integral a => a -> [(a, Int)]
primeFactorsCount = groupCount . primeFactors

intdiv :: (Integral a, Fractional b) => a -> a -> b
intdiv a b = (fromIntegral a) / (fromIntegral b)

totient :: Integral a => [(a,a)] -> a
totient = foldl (\a (p,k) -> a * p^(k-1) * (p-1)) 1

isPermutationOf :: Ord a => [a] -> [a] -> Bool
isPermutationOf a b =
  if (length a) /= (length b) then False
  else (sort a) == (sort b)

isPermutationOfn :: Integral a => a -> a -> Bool
isPermutationOfn a b = isPermutationOf (digits 10 a) (digits 10 b)

target = [2..10^7-1]
  & map (\p -> (p, primeFactorsCount p)) -- get all factors
  & map (\(p, fs) -> (p, totient fs)) -- compute totient
  & filter (uncurry isPermutationOfn) -- keep only permutations
  & map (\(p, t) -> (p, p `intdiv` t)) -- calculate ratio
  & minimumBy (compare `on` snd) -- find minimum

main = print target

{-
-- Compose a 1:1 function onto both arguments of a 2:1 function.
-- Thanks to Hoogle, I found out this is already available as Data.Function.on
compose2 :: (b -> b -> c) -> (a -> b) -> a -> a -> c
compose2 f g = \ x y -> f (g x) (g y)

compareSnd :: Ord a1 => (a2, a1) -> (a3, a1) -> Ordering
compareSnd (_,x) (_,y) = compare x y
-}