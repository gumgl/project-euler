require 'pp'

# Note: test not needed, simply work up from the generated list of triangular numbers
def is_triangular?(i)
	root = Math.sqrt(i*2)

	return (!(root % 1).zero? and 0.5 * root.floor() * root.ceil() == i)
end

def is_pentagonal?(i)
	return (((Math.sqrt(24 * i + 1) + 1) / 6) % 1).zero?
end

def is_hexagonal?(i)
	return (((Math.sqrt(8 * i + 1) + 1) / 4) % 1).zero?
end

pp (1..200).select{|i| is_hexagonal?(i)}
