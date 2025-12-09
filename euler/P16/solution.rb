require 'bigdecimal'

sum = BigDecimal('2')
      .power(1000)
      .to_s('F')[0..-3]
      .each_char.map(&:to_i)
      .reduce(:+)

p sum
