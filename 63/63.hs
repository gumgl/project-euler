import Data.Digits

lend = length . digits 10

bases = [1..9] -- lend (10^x) > 10 for all x
exponents = [1..30] -- lend (9^x) > x for x>30

pairs = [(base,exponent) | base <- bases, exponent <- exponents]

isValid (base,exponent) = (lend (base^exponent) == exponent)

valid = filter isValid pairs

answer = length valid