require 'pp'
# Direction:
# 0 = right (+x)
# 1 = down (+y)
# 2 = left (-x)
# 3 = up (-y)
direction = 0
i = 1
subsize = 1

spiral_size = 5

center_x, center_y = (spiral_size/2).floor(), (spiral_size/2).floor()

diagonal_1, diagonal_2 = [1],[1]

while subsize <= spiral_size
	diagonal_1.push(i)
	i += subsize - 1
	diagonal_1.push(i)
	i += subsize - 1
	diagonal_2.push(i)


	# Write
	grid[center_x][center_y] = i

	subsize += 2


	# Check if we are done
	if ! x.between?(0, spiral_size-1) or ! y.between?(0, spiral_size-1)
		break
	end

	# Increment
	i += 1

	# Change direction
	direction = (direction + 1) % 4

	# Increase step size every two direction changes
	if direction % 2 == 0
		step += 1
	end
end
pp grid
puts (0..spiral_size-1).map{|i| grid[i][i]}.sum
puts (0..spiral_size-1).map{|i| grid[i][spiral_size-1-i]}.sum