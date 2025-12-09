# Naive implementation checking for 4 other cubes amongst a cube's permutations.
# This only runs for n=3. Computing the cube root is neither slow nor fast, but
# there are too many permutations once the cubes get bigger.

def cube?(n)
	return (n != 1 and Math.cbrt(n).floor()**3 == n)
end

cubes = (1..).lazy.map{|i| i**3}

cube_counts = cubes.map{|c|
	c.digits.permutation.select{|p| p[-1] != 0} # Get permutations that don't start with 0
		.map(&:join).map(&:to_i) # Convert back to integers
		.select{|i| cube?(i)}.uniq() # Select unique cubes
	}

puts cube_counts.find{|cp| cp.length == 3}