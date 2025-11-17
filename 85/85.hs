import Data.List (minimumBy)
import Data.Function (on)

-- I first worked out a recursive solution, reducing to a single row.
c :: Int -> Int -> Int
c 1 1 = 1
c 2 1 = 3
c x 1 = x * (x + 1) `div` 2 -- Triangular numbers
c x y
    | x < y = c y x -- only one side of the tree (mirrored)
    | otherwise = (c x (y-1)) -- rectangles from smaller height (excluding bottom line)
        + (c x 1) * y -- rectangles including bottom line

-- I then worked out the closed form equation algebraically...
d x y = (x * (x + 1) `div` 2) * ((y * (y + 1)) `div` 2)

target = minimumBy (compare `on` (\(x, y) -> abs (2000000 - d x y))) [(x, y) | x <- [1..100], y <- [1..x]]

main = print (target,uncurry d target, uncurry (*) target)

-- As it turns out, this is the best solution but for a much simpler and obvious reason which only requires
-- defining rectangles in terms of their 4 sides / coordinates (x+1 choose 2, triangular numbers).