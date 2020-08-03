require 'bigdecimal'

max_a = 100
max_b = 100

puts (1..max_a).map{|a|
		(1..max_b).map{|b|
			BigDecimal(a).power(b) }} # Construct list of a^b
	.flatten # Make it into one big list (size = 100*100)
	.map{|n| n.to_s("F").chars.map(&:to_i)} # Get digits
	.map{|d| d.sum} # Get sums of digits
	.max