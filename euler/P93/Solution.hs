module Euler.P93.Solution where

import qualified Data.Set as Set
import qualified Data.List as List
import Data.Ratio ( (%), numerator, denominator, Ratio )
import Data.Function ( (&), on )

{- Problem size: 967,680 possible expressions to evaluate
 - # of possible picks for a < b < c < d: (9 choose 4) = 126
 - # of possible orderings of {a,b,c,d} = 4! = 24
 - # of possible assignments of operations: 4^3 = 64
 - # of ways to paranthesize an expression: 5
 - Max value of an expression: 9*9*9*9 = 6561
-}

commonPrefixLength :: (Eq a1, Num a2) => [a1] -> [a1] -> a2
commonPrefixLength [] _ = 0
commonPrefixLength _ [] = 0
commonPrefixLength (x:xs) (y:ys) = if x == y
    then 1 + (commonPrefixLength xs ys)
    else 0

type Variable = Ratio Int
type Operation = Variable -> Variable -> Variable
type Expression = (Variable, Variable, Variable, Variable, Operation, Operation, Operation)

-- this is a hack to avoid the issue of division by zero without using exception handling or Maybe
customDiv :: Integral a => Ratio a -> Ratio a -> Ratio a
customDiv a b = if (numerator b) == 0 then 1 % 999999 else a / b
digits :: [Variable]
digits = [1..9]
operations :: [Operation]
operations = [(+),(-),(*),customDiv]

digitSets :: [[Variable]]
digitSets = filter ((==4).length) $ List.subsequences digits

-- Compute all possible values for a given expression.
-- Initially I was drawn to permuting Reverse Polish Notation but went for hardcoding the orders of operation
-- for speed and simplicity, although not as easily scalable for n > 4 (Catalan numbers, next one is 14)
-- See https://en.wikipedia.org/wiki/Catalan_number#Applications_in_combinatorics
values :: Expression -> [Variable]
values (a, b, c, d, o1, o2, o3) = [((a `o1`   b) `o2`  c)  `o3` d  ,
                                     a `o1`  (b  `o2` (c   `o3` d)),
                                    (a `o1`   b) `o2` (c   `o3` d) ,
                                    (a `o1`  (b  `o2`  c)) `o3` d  ,
                                     a `o1` ((b  `o2`  c)  `o3` d) ]

-- Given a set of 4 digits and a set of operations, create a list of expressions.
expressions :: [Variable] -> [Expression]
expressions digitSet = [(a, b, c, d, o1, o2, o3) |
                  a <- digitSet,
                  b <- digitSet, a /= b,
                  c <- digitSet, a /= c, b /= c,
                  d <- digitSet, a /= d, b /= d, c /= d,
                  o1 <- operations, o2 <- operations, o3 <- operations]

answer = digitSets
    & map (\ds -> (ds, (expressions ds)
        & List.concatMap values -- aggregate all values from all expressions for current digit set
        & filter (\v -> (v > 0) && ((denominator v) == 1) -- select only positive integer values
        & map numerator -- from Ratio to Int
        & Set.fromList -- insert values into set
        & Set.elems -- obtain sorted list of unique values
        & commonPrefixLength [1..]) -- count how many match [1..]
        )
    & List.maximumBy (compare `on` snd)

main = print answer