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

input1 :: [(Double, Double)]
input1 = [(57,291),(72,1172),(69,1176),(92,2026)]
input2 :: [(Double, Double)]
input2 = [(57726992, 291117211762026)]

solve = product . map (uncurry countWays)