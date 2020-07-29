# Initial approach:
# 1. build a list of primes smaller than 10^9
# 2. select the pandigital ones
# 3. take the largest
#
# As it turns out, there are too many primes to filter.
# This works quickly for n <= 6 though.

require 'prime'

class Integer
	def is_pandigital?
	  return self.digits.sort == (1..self.digits.length).to_a
	end
end

n = 6 # Start with max n=9

primes_l = Prime.each().lazy

candidates_l = primes_l.select(&:is_pandigital?)

solution = candidates_l.take_while{|c| c < 10**n}.force.last

puts solution