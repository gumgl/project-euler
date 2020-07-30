require 'bigdecimal'

n = 100

puts (1..n).select{|i| Math.sqrt(i).floor()**2 != i} # Select irrationals
	.map{|i| BigDecimal(i).sqrt(n+1)} # Get roots
	.map{|r| r.to_s("F")[0,n+1].tr('.','').chars.map(&:to_i)} # Get digits
	.map{|d| d.sum} # Get sums of digits
	.sum