module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Text.Printf (printf)
import System.CPUTime (getCPUTime)
import Control.Exception (catch, IOException)

import qualified Aoc.Y2023.D06.Solution
import qualified Aoc.Y2024.D01.Solution
import qualified Aoc.Y2024.D02.Solution


-- | Dispatcher
solve :: String -> String -> String -> (String, String)
solve "2023" "06" = Aoc.Y2023.D06.Solution.solve
solve "2024" "01" = Aoc.Y2024.D01.Solution.solve
solve "2024" "02" = Aoc.Y2024.D02.Solution.solve
solve y d = error $ "No solution found for Year " ++ y ++ " Day " ++ d


main :: IO ()
main = do
    args <- getArgs
    case args of
        (year:day:rest) -> do
            let useExample = "-e" `elem` rest || "--example" `elem` rest
            let dayPad = if length day == 1 then "0" ++ day else day
            
            let inputFile = "aoc/Y" ++ year ++ "/D" ++ dayPad ++ "/" ++ (if useExample then "example.txt" else "input.txt")
            
            content <- catch (readFile inputFile)
                             (\e -> die $ "Failed to read input file: " ++ inputFile ++ "\n" ++ show (e :: IOException))
            
            start <- getCPUTime
            let (p1, p2) = solve year dayPad content
            putStrLn $ "Part 1: " ++ p1
            putStrLn $ "Part 2: " ++ p2
            end <- getCPUTime
            let diff = (fromIntegral (end - start)) / (10^9) :: Double
            printf "Execution Time: %.2f ms\n" diff
            
        _ -> die "Usage: aoc <year> <day> [--example]"
