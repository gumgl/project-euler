require 'bigdecimal'

i = BigDecimal("3")
fi2 = BigDecimal("1")
fi1 = BigDecimal("2")

while fi1.to_s('F').length - 2 < 1000
  fi1b = fi1
  fi1 = fi1.add(fi2, 1000)
  fi2 = fi1b
  i = i.add(1, 1000)
end

p [i.to_s('F'), fi1.to_s('F')]
