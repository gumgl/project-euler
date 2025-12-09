def sqrt2(i)
  return i == 0 ? 1 : 2
end

def convergent_sqrt2(i)
  s = Rational(sqrt2(i))
  (i-1).downto(0).each { |j| s = (sqrt2(j)+1r/s) }
  return s
end

def e(i)
  return i==0 ? 2 :
   i % 3 == 2 ? 2 * (i/3+1) :
   1
end

def convergent_e(i)
  s = Rational(e(i))
  (i-1).downto(0).each { |j| s = (e(j)+1r/s) }
  return s
end

puts (0..9).map{|i| convergent_sqrt2(i)}
puts (0..9).map{|i| convergent_e(i)}

c = convergent_e(99)
puts "Solution: %d" % c.numerator.digits.sum
