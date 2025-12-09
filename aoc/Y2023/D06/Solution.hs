module Aoc.Y2023.D06.Solution where

{- https://adventofcode.com/2023/day/6

d(x): distance covered (mm)
x: button press duration (ms)
t: race duration (ms)
r: record (mm)

d(x) = (t-x) * x
r < t*x - x^2
0 < -x^2 + t*x - r

The equation for distance covered as a function of button press is quadratic,
so we simply need to count the number of distinct integer values of x between the two
zeroes obtained via the quadratic formula.

x = (t ± sqrt(t^2 - 4 r)) / 2
-}

bounds :: Floating b => b -> b -> (b, b)
bounds t r = (((t - (sqrt (t^2 - 4 * r))) / 2),
         ((t + (sqrt (t^2 - 4 * r))) / 2))

spread :: (RealFrac a, Integral b) => (a, a) -> b
spread (lower, upper) = (ceiling upper) - (ceiling lower)

countWays t r = spread $ bounds t r

parsePart1 :: String -> [(Double, Double)]
parsePart1 input = zip times dists
  where
    rows = lines input
    parseRow row = map read $ tail $ words row
    times = parseRow (rows !! 0)
    dists = parseRow (rows !! 1)

parsePart2 :: String -> (Double, Double)
parsePart2 input = (time, dist)
  where
    rows = lines input
    parseRow row = read $ concat $ tail $ words row
    time = parseRow (rows !! 0)
    dist = parseRow (rows !! 1)

solve :: String -> (String, String)
solve input = (show p1, show p2)
  where
    p1 = product $ map (uncurry countWays) (parsePart1 input)
    p2 = uncurry countWays (parsePart2 input)