=begin
Similar to #18 and #67
The solution is to work backwards from the end position (bottom right),
growing a square of "solved" minimum path lengths to the end.
Each iteration, we compute the column and row left and above the solved square.
For each of those new squares, we store the smallest path weight below or right
of it, and store it in-place. The starting case (top left) will contain the
weight of the shortest path.
=end

lines = File.readlines('matrix.txt')
matrix = lines.map {|string| string.split(',').map {|n| Integer(n, 10)}}
n = matrix.length # Should be =80

for i in 2..n # For a new square of size 2<i<=n

  # Bottom left of new square:
  matrix[n-1][n-i] += matrix[n-1][n-i+1]
  # Left column of new square:
  for row in (n-2).downto(n-i+1)
    matrix[row][n-i] += [matrix[row+1][n-i], matrix[row][n-i+1]].min
  end
  # Top right of new square:
  matrix[n-i][n-1] += matrix[n-i+1][n-1]
  # Top row of new square:
  for col in (n-2).downto(n-i+1)
    matrix[n-i][col] += [matrix[n-i][col+1], matrix[n-i+1][col]].min
  end

  # Top left of new square:
  matrix[n-i][n-i] += [matrix[n-i+1][n-i], matrix[n-i][n-i+1]].min
end

p matrix[0][0]
