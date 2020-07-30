# Here we cache the cubes and their decimal representation for faster checking
# of whether a cube has enough cube permutations.
# Unfortunately due to the constant searching this is still not very fast.

#def permutations?(i,j)
#	return i.digits.sort == j.digits.sort
#end

cubes = []

for i in (2..)
	matches = cubes.select{|cp| cp[1] == (i**3).digits.sort}
	if matches.length >= 4
		puts matches.map{|m| m[0]} + [i**3]
		break
	end
	cubes.push([i**3,(i**3).digits.sort])
end