from Coordinates import Point

inputfile = open('10_input.txt', 'r')

# Coordinate system: (x,y) where x is column # going right, y is row # going down
#                    representing element grid[y][x]

grid = [[*line.strip()] for line in inputfile.readlines()]
(width, height) = (len(grid[0]), len(grid)) # They happen to be the same

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

def keys_by_value(haystack, predicate_or_value):
  return (key for key, value in haystack.items() 
          if (predicate_or_value(value) if callable(predicate_or_value) else value == predicate_or_value))

def connected_neighbours(pos):
  return (neighbour for move in connections[grid[pos.y][pos.x]]
          if (is_inbounds(neighbour:= pos + move)
              and grid[neighbour.y][neighbour.x] in connections # treat unlisted characters as unconnected
              and move.reverse() in connections[grid[neighbour.y][neighbour.x]]))

def find_cycle():
  path = [next(indices_2d(grid, 'S'))]
  path.append(next(connected_neighbours(path[0]))) # force the first move to the first connection (see `dirs` for order)

  while grid[path[-1].y][path[-1].x] != 'S':
    path.append(next((neighbour for neighbour in connected_neighbours(path[-1]) if neighbour != path[-2])))

  return path

# Brute force strategy: Parse all rows left to right, keeping track of path crossings and counting inside squares
def count_outside_area(path):
  inside_area = 0
  inside = False
  upWillCross = False # at the end of a horizontal pipe section, we must know if we will have crossed the path: ┏━┓ vs ┏━┛

  # Optimizes from O(n^2) to O(n) (assuming linear search, n is grid area), speeding execution by a factor of ~80 (measured)
  path_area_cache = [[False for _ in range(height)] for _ in range(width)]
  for pos in path:
    path_area_cache[pos.y][pos.x] = True

  for y, line in enumerate(grid):
    for x, char in enumerate(line):
      pos = Point(x,y)

      # If S is a corner at the end of a series of '-', we cannot tell its direction
      # Note: this edge case is not present in my input but I'm covering it for the sake of completeness
      # Tested by moving S to a corner
      if char == 'S':
        char = next(keys_by_value(connections, [neighbour - pos for neighbour in connected_neighbours(pos)]))

      if path_area_cache[pos.y][pos.x]:
        match char:
          case '|':
            inside = not inside
          case 'L':
            upWillCross = False
          case 'F':
            upWillCross = True
          case 'J':
            if upWillCross: # up will cross path
              inside = not inside # flip
          case '7':
            if not upWillCross: #down will cross path
              inside = not inside # flip
          case 'S':
            #Special case to handle when S is a corner
            inside = not inside

      elif inside:
          inside_area += 1

  return inside_area

def part_1():
  return len(find_cycle()) // 2

def part_2():
  return count_outside_area(find_cycle())

print(part_1())
print(part_2())

# Other strategies for part 2:
# (1) Increase space between pipes to enable flood fill algorithm. Double grid resolution, copy previous connectors in top-left square then extend connections. O(grid area)
# (2) Use the shoelace formula and Pick's theorem to measure area geometrically. Surprisingly clean in practice. Most efficient. O(path length)