$denominations = {
  'M' => 1000,
  'D' => 500,
  'C' => 100,
  'L' => 50,
  'X' => 10,
  'V' => 5,
  'I' => 1}

def numeral_to_i(numeral)
  sum = 0
  i = 0
  while i < numeral.length
    if i < numeral.length-1 &&
      $denominations.keys.index(numeral[i+1]) < $denominations.keys.index(numeral[i])
      # we have a subtractive combination
      sum += $denominations[numeral[i+1]] - $denominations[numeral[i]]
      i += 2
    else
      sum += $denominations[numeral[i]]
      i += 1
    end
  end
  return sum
end

def i_to_numeral(n)
  numeral = ""
  for i in (0..$denominations.length-1) do
    denomination = $denominations.keys[i]

    if ['D', 'L', 'V'].include? denomination
      four = 4 * $denominations.values[i+1]
      nine = 9 * $denominations.values[i+1]
      #puts nine
      if n >= nine then
        numeral << $denominations.keys[i+1] + $denominations.keys[i-1]
        n -= nine
      elsif n >= four && n < $denominations.values[i]
        numeral << $denominations.keys[i+1] + $denominations.keys[i]
        n -= four
      end
    end

    while n >= $denominations[denomination] do
      numeral << denomination
      n -= $denominations[denomination]
    end
  end
  return numeral
end

#[21,24,36,40,49,50,87,99,101,110,130,200,333,499,500,501,700,920,1002,2555,7000].each {|i| puts "%d(%d): %s" % [i,numeral_to_i(i_to_numeral(i)),i_to_numeral(i)]}
solution = 0
File.readlines("roman.txt").map {|line| line.strip}.each {|numeral|
  #puts "%s => %s, %d characters saved" % [numeral, i_to_numeral(numeral_to_i(numeral)), numeral.length - i_to_numeral(numeral_to_i(numeral)).length]
  solution += numeral.length - i_to_numeral(numeral_to_i(numeral)).length
}
puts "%d characters saved" % solution
