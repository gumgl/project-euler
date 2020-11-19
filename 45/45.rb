# Using the quadratic formula, we can derive closed-form
# tests for these properties.

# Note: Triangular test not needed but included for completeness
def is_triangular?(i)
	return (Math.sqrt(8 * i + 1) % 1).zero?
end

def is_pentagonal?(i)
	return (((Math.sqrt(24 * i + 1) + 1) / 6) % 1).zero?
end

def is_hexagonal?(i)
	return (((Math.sqrt(8 * i + 1) + 1) / 4) % 1).zero?
end

triangle_l = (1..).lazy.map{|n| n*(n+1)/2}

solutions_l = triangle_l.select{|n| is_pentagonal?(n) and is_hexagonal?(n)}

puts solutions_l.find{|n| n > 40755}