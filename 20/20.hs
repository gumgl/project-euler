import Data.Digits

fact 1 = 1
fact n = n * fact (n - 1)

answer = sum $ digits 10 (fact 100)
