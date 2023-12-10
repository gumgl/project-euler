from Coordinates import Point

inputfile = open('10_input.txt', 'r')

# Coordinate system: (x,y) where x is column # going right, y is row # going down
#                    representing element grid[y][x]

grid = [[*line.strip()] for line in inputfile.readlines()]
(width, height) = (len(grid[0]), len(grid))

dir = {
  'UP': Point(0,-1),
  'RIGHT': Point(1,0),
  'DOWN': Point(0,1),
  'LEFT': Point(-1,0)}

connections = {
  '|': [dir['UP'], dir['DOWN']],
  '-': [dir['LEFT'], dir['RIGHT']],
  'L': [dir['UP'], dir['RIGHT']],
  'J': [dir['UP'], dir['LEFT']],
  '7': [dir['LEFT'], dir['DOWN']],
  'F': [dir['RIGHT'], dir['DOWN']],
  '.': [],
  'S': dir.values()}

def is_inbounds(point):
  return 0 <= point.x < width and 0 <= point.y < height

# Using iterators for laziness i.e. stop searching if caller only needs one element
def indices_2d(haystack, predicate_or_value):
  return (Point(x,y) for (y,line) in enumerate(haystack) for (x,e) in enumerate(line)
          if (predicate_or_value(e) if callable(predicate_or_value) else e == predicate_or_value))

def connected_neighbours(pos):
  return (neighbour for move in connections[grid[pos.y][pos.x]]
          if is_inbounds(neighbour:= pos + move) and move.reverse() in connections[grid[neighbour.y][neighbour.x]])

def cycle_length():
  prev_pos = next(indices_2d(grid, 'S'))
  curr_pos = next(connected_neighbours(prev_pos)) # force the first move to the first connection (see `dir` for order)
  length = 1

  while grid[curr_pos.y][curr_pos.x] != 'S':
    length += 1
    (prev_pos, curr_pos) = (curr_pos, next((neighbour for neighbour in connected_neighbours(curr_pos) if neighbour != prev_pos)))

  return length

def part_1():
  return int(cycle_length()/2)

print(part_1())