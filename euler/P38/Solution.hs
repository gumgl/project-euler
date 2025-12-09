module Euler.P38.Solution where

import Data.List
import Data.Digits

isPandigital n =
  let d = digits 10 n
  in (sort d) == [1..(length d)]

is9Pandigital n = (isPandigital n) && ((length $ digits 10 n) == 9)

concatProduct m n = unDigits 10 $ concat $ map ((digits 10) . (m*)) [1..n]

isTarget m n = is9Pandigital $ concatProduct m n

-- targets must have 9 digits
candidatesM = [9..50000] -- for smallest n=2, 
candidatesN = [2..5] -- for smallest m=9, n=5

targets = [concatProduct m n | m<-candidatesM, n<-candidatesN, isTarget m n]

answer = maximum targets