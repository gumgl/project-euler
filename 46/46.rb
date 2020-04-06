require 'prime'

def is_double_square(n)
  return Math.sqrt(n/2) % 1 == 0
end

primes = Prime.each()
primes_array = [primes.next] # store primes as we go
prime = primes.next # start sieve with 3

for i in 1.. do
  n = 2*i+1 # start with odd numbers

  if n == prime then # we've hit a prime from the sieve
    primes_array.push(prime) # so we check it on further composite numbers
    prime = primes.next
  else # composite number n
    can_be_done = false
    for q in primes_array do
      if is_double_square(n - q) then
        can_be_done = true
        break
      end
    end
    if !can_be_done then
      puts "Solution: %d" % n
      break
    end
  end
end
