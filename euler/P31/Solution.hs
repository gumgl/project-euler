module Euler.P31.Solution where

engcoins = [200, 100, 50, 20, 10, 5, 2, 1]

count _ 0 = 1
count [] _ = 0
count (coin:coins) money = sum $ map (\i -> count coins (money - i*coin)) [0 .. (money `div` coin)]

answer = count engcoins 200
