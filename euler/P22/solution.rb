require 'json'

input = File.read("names.txt")
names = JSON.parse("[" + input + "]").sort

score = 0
for i in (0..names.length-1)
  #puts "%s: %d * %d" % [names[i], i+1, names[i].chars.map {|c| c.ord - 'A'.ord + 1}.sum]
  score += (i+1) * names[i].chars.map {|c| c.ord - 'A'.ord + 1}.sum
end

puts score
