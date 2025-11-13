import Data.List (sort)
import Solution

main :: IO ()
main = runSolution $ Day
    { dayNumber = "01"
    , solvePart1 = part1
    , solvePart2 = part2
    }

part1 :: String -> String
part1 content =
    let pairs = map parseLine (lines content)
        lefts = sort [left | (left, _) <- pairs]
        rights = sort [right | (_, right) <- pairs]
        differences = zipWith absoluteDifference lefts rights
        totalDiff = sum differences
    in show totalDiff

part2 :: String -> String
part2 content =
    let pairs = map parseLine (lines content)
        lefts = [left | (left, _) <- pairs]
        rights = [right | (_, right) <- pairs]
        similarity = map (\l -> l * count l rights) lefts
        total = sum similarity
    in show total
parseLine :: String -> (Int, Int)
parseLine line =
    let parts = words line
        left = read (parts !! 0) :: Int
        right = read (parts !! 1) :: Int
    in (left, right)
