require 'json'

def word_value(word)
	return word.chars.map{|c| c.ord - 'A'.ord + 1}.sum
end

def is_triangular?(i)
	root = Math.sqrt(i*2)

	return (!(root % 1).zero? and 0.5 * root.floor() * root.ceil() == i)
end

input = File.read("words.txt")
words = JSON.parse("[" + input + "]")

puts words.count{|w| is_triangular?(word_value(w))}