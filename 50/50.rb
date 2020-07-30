require 'prime'

solution_limit = 10**6

start_primes = Prime.each()

best_prime = 1
best_length = 1
best_start = 1

while true # For each start of a sequence
	start = start_primes.next # Advance start_primes
	sum = start
	length = 1
	sequence = start_primes.clone

	if sum >= solution_limit
		break
	end

	while sum < solution_limit
		# Note the first part of the following condition makes us skip
		# prime checks when length is suboptimal
		if length > best_length and sum.prime?
			best_length = length
			best_prime = sum
			best_start = start
		end
		sum += sequence.next
		length += 1
	end
end

puts "sum"
print Prime.each().lazy.select{|p| p > best_start}.take(best_length).to_a
puts " = " , best_prime
puts "length = ", best_length