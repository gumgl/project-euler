module Euler.P719.Solution where

import Data.Digits
import Data.Function

-- Problem chosen in order to unlock "Easy Prey" award https://projecteuler.net/award=51

{- A group here is a way to split a list l into sublists that concatenated together equal l
  This function lists all possible groups.
  e.g. groups "abc" == [["abc"],["ab","c"],["a","bc"],["a","b","c"]]
-}
groups         :: [a] -> [[[a]]]
groups []      = [[[]]]
groups [x]     = [[[x]]]
groups (x:[y]) = [[[x,y]],[[x],[y]]]
groups (x:xs)  = (map (\(y:ys) -> (x:y):ys) tailgroups) -- for all tailgroups add x to first group
                    ++ (map ([x]:) tailgroups) -- for all tailgroups prepend x as its own group
                    where tailgroups = groups xs

isSNumber n = (digits 10) (n^2)
            & groups -- all possible groups of digits
            & (map . map) (unDigits 10) -- transform back to numbers
            & map sum -- sum them
            & any (== n) -- if any matches, it's an S-number

t n = [x | x <- [2..n], (x `mod` 9) `elem` [0,1]] -- where n = n^2 (mod 9)
    & filter isSNumber
    & map (^2)
    & sum

answer = t (10^6)

main = print answer