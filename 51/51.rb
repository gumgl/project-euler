=begin
Here we are looking for the smallest number in the set of numbers
that have properties A & B where
A: prime numbers
B: numbers that are part of a 8-family
Instead of starting from the really large list of numbers in B and
finding which ones are prime, we start from prime numbers and find
which ones make up an entirely-prime 8-family.
We map every prime number to a star-format number which could have
generated it e.g. from 56883 to 56**3. Note that since all the stars
can only be replaced by the same digit, when we do the inverse, we
can only select a subset of the same digits to replace by stars.
=end

require 'prime'

# For a given array a, returns all the subsets of a
def get_all_subsets(a)
	return (1..a.length).map{|i| a.combination(i).to_a}
	.flatten(1)
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
			get_all_subsets(locations) # Get all subsets
			.select{|locations_subset| locations_subset.length < n_string.length }
			# Filter out cases where we want to replace the entire number
		}.flatten(1) # Here we have a list of star locations regardless of digit
		.map{|locations_subset| # Put a star in every location in a duplicate of n
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