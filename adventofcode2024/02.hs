import Solution

main :: IO ()
main = runSolution $ Day
    { dayNumber = "02"
    , solvePart1 = part1
    , solvePart2 = part2
    }

part1 :: String -> String
part1 content =
    let levels = map parseLine (lines content)
        answer = countIf isSafe levels
    in show answer

part2 :: String -> String
part2 content =
    let levels = map parseLine (lines content)
        answer = countIf isSafeCanRemove levels
    in show answer

parseLine :: String -> [Int]
parseLine line = map read $ words line :: [Int]

isSafe :: [Int] -> Bool
isSafe level = 
    let diffs = consecutiveMap (-) level
    in all (<? (1, 3)) diffs || all (<? (-3, -1)) diffs

isSafeCanRemove :: [Int] -> Bool
isSafeCanRemove level = any isSafe $ combinations (length level - 1) level