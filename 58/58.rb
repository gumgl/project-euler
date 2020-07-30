# Similar to #28

require 'prime'

step = 1
size = 1
count_primes = 0
ratio = 1

while ratio >= 0.1
	size = 2 * step + 1
	base = (2 * step - 1)**2
	corners = (1..4).map{|i| base + i * (size - 1)}
	count_primes += corners.count(&:prime?)
	count_total = 1+4*step
	ratio = 1.0*count_primes/count_total
	#print "%s: %d/%d = %2.2f%%, size=%d\n" %
	#	[corners.to_s, count_primes, count_total, 100*ratio, size]
	step += 1
end
puts size

#1
#3,5,7,9       +2  = 1*4 + 20  i=1
#13,17,21,25   +4  = 3^2*4 + 40  i=2
#31,37,43,49   +6  = 5^2*4 + 60 i=3