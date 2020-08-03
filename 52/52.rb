def isSolution?(n)
	return (1..6).map{|e| n*e}
		.map{|e| e.to_s.chars.sort}
		.uniq.size == 1
end

n = (1..).find{|n| isSolution?(n)}

puts (1..6).map{|e| n*e}