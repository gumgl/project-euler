import Data.Digits
--import Data.List
import qualified Data.Map as Map
import Debug.Trace
cacheSize = 100


--cache = newListArray []

--isAMatch n =
--    

nextN :: Int -> Int
nextN n = do {-trace ("[n=" ++ show n ++ "]")-} (sum $ map (^2) (digits 10 n))

memoized_endOfChain :: Int -> Bool
memoized_endOfChain = (map endOfChain [0 ..] !!)
    where endOfChain 1 = False
          endOfChain 89 = True
          endOfChain n = memoized_endOfChain (nextN n)

--a chain starting at n 
chain n =
    if (n == 89 || n == 1)
    then [n]
    else n : chain (nextN n)

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

answerI = length $ filter (\i -> (last (chain i)) == 89) [1..10^7-1]
answerE = length $ filter memoized_endOfChain [1..10^7-1]

--answer = countMatches [1..10^7-1]
