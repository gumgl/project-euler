module Euler.P36.Solution where

import Data.Digits

isPalindrome xs = (xs == reverse xs)

isDblPal x = ((isPalindrome (digits 10 x)) && (isPalindrome (digits 2 x)))

answer = sum $ filter isDblPal [1..999999]