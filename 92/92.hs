import Data.Digits
--import Data.List
--import qualified Data.Map as Map
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Debug.Trace

-- highest step possible
cacheSize = 7 * 9^2

step :: Int -> Int
step n = do {-trace ("[n=" ++ show n ++ "]")-} (sum $ map (^2) (digits 10 n))

endsWith89 :: IORef (IntMap.IntMap Bool) -> Int -> IO Bool
endsWith89 _ 1 = pure False
endsWith89 _ 89 = pure True
endsWith89 ref n = do
    m <- readIORef ref
    case IntMap.lookup n m of
        (Just v) -> pure v
        Nothing -> do
            v <- endsWith89 ref (step n)
            writeIORef ref (IntMap.insert n v m)
            pure v

--a chain starting at n 
chain n =
    if (n == 89 || n == 1)
    then [n]
    else n : chain (step n)

--answerI = length $ filter (\i -> (last (chain i)) == 89) [1..10^7-1]
--answerE = length $ filter memoized_endOfChain [1..10^7-1]

main :: IO ()
main = do
    ref <- newIORef (IntMap.empty)-- :: IntMap.IntMap Bool)
    answer <- length <$> filterM (endsWith89 ref) [1..9999999]
    print answer
    --readIORef ref >>= print
