sum = 0
File.foreach("numbers.txt") {|line| sum += Integer(line, 10)}

puts sum
