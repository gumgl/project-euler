from Coordinates import Point

inputfile = open('10_input.txt', 'r')

# Coordinate system: (x,y) where x is column # going right, y is row # going down
#                    representing element grid[y][x]

grid = [[*line.strip()] for line in inputfile.readlines()]
(width, height) = (len(grid[0]), len(grid))

dirs = {
  'UP': Point(0,-1),
  'RIGHT': Point(1,0),
  'DOWN': Point(0,1),
  'LEFT': Point(-1,0)}

connections = {
  '|': [dirs['UP'], dirs['DOWN']],
  '-': [dirs['LEFT'], dirs['RIGHT']],
  'L': [dirs['UP'], dirs['RIGHT']],
  'J': [dirs['UP'], dirs['LEFT']],
  '7': [dirs['LEFT'], dirs['DOWN']],
  'F': [dirs['RIGHT'], dirs['DOWN']],
  'S': dirs.values()}

def is_inbounds(point):
  return 0 <= point.x < width and 0 <= point.y < height

# Using iterators for laziness i.e. stop searching if caller only needs one element
def indices_2d(haystack, predicate_or_value):
  return (Point(x,y) for (y,line) in enumerate(haystack) for (x,e) in enumerate(line)
          if (predicate_or_value(e) if callable(predicate_or_value) else e == predicate_or_value))

def connected_neighbours(pos):
  return (neighbour for move in connections[grid[pos.y][pos.x]]
          if is_inbounds(neighbour:= pos + move) and move.reverse() in connections[grid[neighbour.y][neighbour.x]])

def find_cycle():
  path = [next(indices_2d(grid, 'S'))]
  path.append(next(connected_neighbours(path[0]))) # force the first move to the first connection (see `dirs` for order)

  while grid[path[-1].y][path[-1].x] != 'S':
    path.append(next((neighbour for neighbour in connected_neighbours(path[-1]) if neighbour != path[-2])))

  return path

def part_1():
  return int(len(find_cycle())/2)

print(part_1())