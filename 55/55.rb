def reverseNumber(n)
	return n.to_s.reverse.to_i
end

def isPalindrome?(n)
	return n == reverseNumber(n)
end

def isLychrel?(n)
	max_iterations = 50
	i = 0
	while i < max_iterations
		i += 1
		n = n + reverseNumber(n)

		if isPalindrome?(n) # Placed here so we don't check first n
			return false
		end
	end
	return true
end

puts (1..9999).count{|n| isLychrel?(n)}