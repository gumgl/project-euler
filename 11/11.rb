require 'pp'

class Candidate
  attr_accessor :value, :members
  def initialize(c)
    @members = c
    @value = c[0] * c[1] * c[2] * c[3]
  end
end

lines = File.readlines("numbers.txt")
numbers = lines.map {|string| string.split(' ').map {|n| Integer(n, 10)}}
grid_size = 20
biggest = Candidate.new([0,0,0,0])

def check(biggest, candidates)
  candidate = Candidate.new(candidates)
  return (candidate.value > biggest.value) ? candidate : biggest
end

for row in 0..grid_size-1
  for col in 0..grid_size-1
    #p [row, col]
    #p biggest.value
    if row >= 3 then # Vertical
      biggest = check(biggest, [numbers[row-3][col], numbers[row-2][col], numbers[row-1][col], numbers[row][col]])
    end
    if col >= 3 then # Horizontal
      biggest = check(biggest, [numbers[row][col-3], numbers[row][col-2], numbers[row][col-1], numbers[row][col]])
    end

    if row >= 3 && col >= 3 then # Diagonal down
      biggest = check(biggest, [numbers[row-3][col-3], numbers[row-2][col-2], numbers[row-1][col-1], numbers[row][col]])
    end
    if row >= 3 && col <= grid_size -4 then # Diagonal up
      biggest = check(biggest, [numbers[row-3][col+3], numbers[row-2][col+2], numbers[row-1][col+1], numbers[row][col]])
    end
  end
end

pp biggest
