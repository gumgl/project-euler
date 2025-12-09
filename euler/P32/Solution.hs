module Euler.P32.Solution where

import Data.List
import Data.List.Extra
import Data.Digits

isPandigital n =
  let d = digits 10 n
  in (sort d) == [1..(length d)]

is9Pandigital n = (isPandigital n) && ((length $ digits 10 n) == 9)

isTarget m n = is9Pandigital (unDigits 10 ((digits 10 m)++(digits 10 n)++(digits 10 (m*n))))

candidatesA = [2..100] -- 100*100=10000 => too many digits
candidatesB = [2..5000]

targets = nubOrd [ a*b | a<-candidatesA, b<-candidatesB, a<b, isTarget a b]

answer = sum targets