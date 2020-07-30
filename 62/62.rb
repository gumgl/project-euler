# Much faster approach by caching the number of cube permutations for a
# given cube.

# We could have stored the smallest too, but have to rule out leading 0s.
def largest_permutation(i)
	return i.to_s.split('').map(&:to_i).sort.reverse.join.to_i
end

counts = Hash.new(0)
smallest_permutation = Hash.new(0)

for i in (2..)
	key = largest_permutation(i**3)
	counts[key] += 1
	
	if counts[key] == 1
		smallest_permutation[key] = i**3
	elsif counts[key] == 5
		puts smallest_permutation[key]
		break
	end
end