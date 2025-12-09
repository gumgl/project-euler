def maximum_path_sum(filename)
  lines = File.readlines(filename)
  triangle = lines.map {|string| string.split(' ').map {|n| Integer(n, 10)}}

  (triangle.length-2).downto(0).each {|row|
    for col in 0..row
      triangle[row][col] = triangle[row][col] + [triangle[row+1][col], triangle[row+1][col+1]].max
    end
  }

  return triangle[0][0]
end
puts "Problem 18 solution: %d" % maximum_path_sum("../18/triangle.txt")
puts "Problem 67 solution: %d" %  maximum_path_sum("triangle.txt")
