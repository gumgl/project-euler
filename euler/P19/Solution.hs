module Euler.P19.Solution where

{- Instead of using a calendar library and a published algorithm for
calculating the day of the week, I decided to write my own closed-form
formula for the starting day of a given month, and just count the Sundays.

I am aware that there are many ways to improve runtime, verbosity, clarity and generality.
-}

(startYear, endYear) = (1901,2000)
startOffset = 1 -- Jan 1 1901 was a Tuesday
targetDay = 6 -- searching for Sundays

monthLengths = [31,28,31,30,31,30,31,31,30,31,30,31]
monthLengthsLeap = 31:29:(drop 2 monthLengths)

(yearlyOffset, yearlyOffsetLeap) = (1,2)
--map ((`mod` 7) . sum) [monthLengths, monthLengthsLeap]

isLeap y =
  y `rem` 4 == 0 &&
  (y `rem` 100 /= 0 ||
    y `rem` 400 == 0)

-- not including end year
-- for the 20th century you can just = floor(years/4)
countLeapYears start end = length $ filter isLeap [start..end-1]
countCommonYears start end = (end - start - countLeapYears start end)

offsetForYear y = (countLeapYears startYear y) * yearlyOffsetLeap
                + (countCommonYears startYear y) * yearlyOffset
                + startOffset

offsetForMonth y m = 
    ((sum . (take m) $ if isLeap y then monthLengthsLeap else monthLengths)
    + offsetForYear y) `mod` 7

answers = [(year, month) |
  year <- [startYear..endYear],
  month <-[0..11],
  offsetForMonth year month == targetDay]

answer = length answers

main = print answer