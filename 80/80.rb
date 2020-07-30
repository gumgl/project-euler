require 'bigdecimal'

numbers = 100
precision = 1000

puts (1..numbers).select{|i| Math.sqrt(i).floor()**2 != i} # Select irrationals
	.map{|i| BigDecimal(i).sqrt(precision+1)} # Get roots
	.map{|r| r.to_s("F")[0,precision+1].tr('.','').chars.map(&:to_i)} # Get digits
	.map{|d| d.sum} # Get sums of digits
	.sum