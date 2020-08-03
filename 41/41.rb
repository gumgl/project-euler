# Better approach:
# 1. build a list of n-digit pandigital numbers for n=1..9
#    sum_(n=1)^9 n! = 409,113 candidates
# 2. find the largest that is also a prime

require 'prime'

max_n=9

pandigitals = (1..max_n)
			.map{|n| (1..n).to_a.permutation.map(&:join).map(&:to_i)}
			.flatten(1)

candidates = pandigitals.select(&:prime?)

puts candidates.last