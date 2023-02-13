import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Function ( (&) )

-- # of 4-digit polygonal numbers with no zeros as 3rd character:
-- ghci> let cs = map (length . candidates) [3..8]
-- cs == [88,53,47,44,40,30]
-- sum cs == 302

filterRange :: Ord a => a -> a -> [a] -> [a]
filterRange = filterRangeOn id

-- Assumes a sorted list
filterRangeOn :: Ord a => (b -> a) -> a -> a -> [b] -> [b]
filterRangeOn f lowerBound upperBound = takeWhile ((< upperBound) . f) . dropWhile ((< lowerBound) . f)

firstTwoDigits :: Int -> Int
firstTwoDigits = (`div` 100)
lastTwoDigits :: Int -> Int
lastTwoDigits = (`mod` 100)
thirdDigit :: Int -> Int
thirdDigit = (`div` 10) . (`mod` 100)

p :: Int -> [Int]
p s = map (\n -> (((s - 2) * n * (n - 1)) `div` 2) + n) [1..]

candidates :: Int -> [Int]
candidates = filter ((/= 0) . thirdDigit) . filterRange (10^3) ((10^4)-1) . p

candidatesTagged :: Int -> [(Int, Int)]
candidatesTagged s = map (s,) $ candidates s

otherCandidates :: [(Int, Int)]
otherCandidates = sortBy (comparing snd) $ concatMap candidatesTagged [4..8]

chains :: Int -> [(Int, Int)] -> [[Int]]
chains n [] = [[n]]
chains n remainingCandidates =
  filterRangeOn snd (100 * lastTwoDigits n) ((100* (lastTwoDigits n + 1)) - 1) remainingCandidates & -- for all who can follow in the chain
    concatMap (\(s, c) -> map (n:) (chains c (filter ((/= s).fst) remainingCandidates))) -- remove current polygon size, find subchains, append n and aggregate


chain = candidates 3 &
  concatMap (`chains` otherCandidates) &
  filter ((== 6) . length) & 
  filter (\l -> (lastTwoDigits $ last l) == (firstTwoDigits $ head l))

answer = sum $ head chain