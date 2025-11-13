module Solution (runSolution, Day(..), module Common) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (isPrefixOf)
import Common

data Day = Day 
    { dayNumber :: String
    , solvePart1 :: String -> String
    , solvePart2 :: String -> String
    }

runSolution :: Day -> IO ()
runSolution day = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Usage: runhaskell 01.hs [s|i]"
            putStrLn "  s - run with sample input"
            putStrLn "  i - run with full input"
            exitFailure
        (fileType:_) -> do
            let filename = case fileType of
                    "s" -> dayNumber day ++ "_sample.txt"
                    "i" -> dayNumber day ++ "_input.txt"
                    _ -> error "Invalid argument. Use 's' for sample or 'i' for input."
            content <- readFile filename
            let answer1 = solvePart1 day content
                answer2 = solvePart2 day content
            putStrLn $ "Day " ++ dayNumber day ++ " - Sample" ++ (if fileType == "i" then " Input" else "")
            putStrLn $ "Part 1: " ++ answer1
            putStrLn $ "Part 2: " ++ answer2
