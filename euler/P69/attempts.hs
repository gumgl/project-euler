module Euler.P69.Attempts where

import qualified Data.Map as Map
import Control.Monad.State

-- Attempt at using Data.Map.fromList:

tooBig (m,n) = m > 10^6

coprimePairsUntil :: Integral a => ((a,a) -> Bool) -> [(a, a)]
coprimePairsUntil stopF = (branch (2,1)) ++ (branch (3,1))
  where branch (m,n) = if stopF (m,n) then [(m,1)]
                       else (m,1)
                :  (branch (2*m-n,m))
                ++ (branch (2*m+n,m))
                ++ (branch (m+2*n,n))

countsOld = Map.fromListWith
  (+)
  (coprimePairsUntil tooBig)

-- Attempt at using Data.Map.unions, which is not much faster

coprimeMapUntil :: ((Int,Int) -> Bool) -> Map.Map Int Int
coprimeMapUntil stopF = Map.unionWith (+) (branch 2 1) (branch 3 1)
  where branch m n = if stopF (m,n) then Map.empty
                     else Map.insertWith (+) m 1 $ -- Add current m to the 3 submaps
                          Map.unionsWith (+) [
                          (branch (2*m-n) m),
                          (branch (2*m+n) m),
                          (branch (m+2*n) n)]

counts = coprimeMapUntil tooBig

--answer = map (counts Map.!) [2..10]

-- Playing around with the State monad, but didn't get to add 

type CoprimeCount = Map.Map Int Int

push :: (Int,Int) -> State CoprimeCount ()  
push (m,n) = state $ \counts -> ((),Map.insertWith (+) m 1 counts)

printMap :: State CoprimeCount String
printMap = state $ \counts -> (show . Map.toList $ counts, counts)

mapManip :: State CoprimeCount String
mapManip = do
  push (4,3)
  push (5,8)
  push (5,6)
  printMap

--answer = runState mapManip Map.empty