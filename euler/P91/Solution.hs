module Euler.P91.Solution where

import Data.Ratio

rightTriangles maxN = [((x1, y1), (x2, y2))
            | x1 <- [0..maxN], x2 <- [0..maxN], y1 <- [0..maxN], y2 <- [0..maxN],
              -- exclusive of:
              (x1, y1) < (x2, y2), -- two distinct points, avoiding duplicate assignments
              (x1, y1) /= (0, 0), (x2, y2) /= (0, 0), -- not at the origin
              (x1, x2) /= (0, 0), (y1, y2) /= (0, 0), -- not both on same axis
              -- inclusive of:
              (x1, y2) == (0, 0) || (x2, y1) == (0, 0) -- right angle at origin
              || ((y1 == 0 || y2 == 0) && x1 == x2) -- right angle on x axis
              || ((x1 == 0 || x2 == 0) && y1 == y2) -- right angle on y axis
              || (x2 /= x1 && y1 /= 0 && ((y2 - y1) % (x2 - x1)) == (- x1) % y1 ) -- right angle at P (x1, y1)
              || (x2 /= x1 && y2 /= 0 && ((y2 - y1) % (x2 - x1)) == (- x2) % y2 ) -- right angle at Q (x2, y2)
              ]

main = print $ length $ rightTriangles 50