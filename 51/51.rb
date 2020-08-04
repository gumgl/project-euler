=begin
Here we are looking for the smallest number which has the following
2 properties:
A: prime number
B: part of an 8-family
Instead of starting from the really large list of numbers in B and
finding which ones are prime, we start from A and find
which ones make up an entirely-prime 8-family.
We map every prime number to star formats which can generate it
e.g. from 56883 to 56**3. Note that since all the stars can only
be replaced by the same digit, when we do the inverse, we can only
select a subset of the same digits to replace by stars.

Restrictions on candidate star formats
0: All-stars are not allowed per problem description.
1: The last digit cannot be a star since it would create even numbers.
2: The number of stars should be a multiple of 3, since increasing 3*n
   digits by 1 will increase the sum of digits by 3 every time, and
   any other number of stars will cycle to sums divisible by 3, hence
   non-primes, therefore any star format like that is doomed.
   Note: this is only for n >= 8, because we can afford 3 multiples of 
   3 otherwise.
=end

require 'prime'

# For a given array a, returns all the subsets of a given size
def get_all_subsets(a, size = nil, multiple = nil)
	if size == nil # All subsets of all sizes
		return (1..a.length).map{|i| a.combination(i).to_a}
		.flatten(1)
	elsif multiple == nil or multiple == false # All subsets of given size
		return a.combination(size).to_a
	else # All subsets of size multiple (bad choice of variable names...) 
		return (size..a.length).step(size).map{|i| a.combination(i).to_a}
		.flatten(1)
	end
end

# Returns a list of positions where c occurs in s
def char_occurences(s, c)
	return (0...s.length).find_all{|i| s[i] == c}
end

# Returns all possible star formats that could result in n
def get_stars(n)
	n_string = n.to_s
	stars_list = n_string.chars.uniq # For each unique digit in n
		.map{|d|
		char_occurences(n_string, d)} # Get its locations in n
		.map{|locations|
			get_all_subsets(locations, 3, true) # Get all subsets, restriction 2
			.select{|locations_subset| 
				locations_subset.length < n_string.length and # Restriction 0
				locations_subset.last != n_string.length - 1 # Restriction 1
			}
		}.flatten(1) # Here we have a list of star locations regardless of digit
		.map{|locations_subset| # Put a star in every location in n
			n_stars = n_string.dup
			locations_subset.each{|i|
				n_stars[i] = "*"}
			n_stars
		}
	return stars_list
end

# Returns the star format of the smallest prime which is part of
# a prime value family of size family_size
def find_smallest_n(family_size)
	counts = Hash.new(0)

	for p in Prime.each()
		for permutation in get_stars(p)
			counts[permutation] += 1
			if counts[permutation] == family_size
				return permutation
			end
		end
	end
end

n_star = find_smallest_n(8)

puts n_star

# Bonus: output members of the family
puts (0..9).map{|d| n_star.gsub("*", d.to_s).to_i}
	.select(&:prime?)