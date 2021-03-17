# Attempt/draft

#1504170715041707n mod 
#4503599627370517

#1_299_709.pow(1_300_751, 104_729)
=begin
step = 1504170715041707
term = step
lowest = term
sum = term

modulus = 4503599627370517

while lowest > 1**6
	term = (term + step).modulo(modulus)

	if term < lowest # Eulercoin found
		sum += term
		lowest = term
		puts term
	end
end

puts "Sum = #{sum}"
=end
e=1504170715041707
mod=4503599627370517
tot=e
min=e
max=e

while true
	if min == 1
		break
	end
	v = min + max
	v = v % mod
	if v > max
		max = v
	end
	if v < min
		min = v
	end
	tot += min
end
print("TOTAL:",tot)