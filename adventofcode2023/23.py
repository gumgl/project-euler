from Coordinates import Point

input_file = open('23_input.txt', 'r')
input_grid = [list(*line.split()) for line in input_file.read().splitlines()]

slope_directions = {
  '>': Point(1, 0),
  'v': Point(0, 1),
  '<': Point(-1, 0),
  '^': Point(0, -1)}
possible_directions = slope_directions.values()

def size(grid):
  return Point(len(grid[0]), len(grid))

def build_graph(grid, start, end, slippery = False):
  """Builds a graph of connections from key to values along with their segment lengths"""
  intersections = {intersection for y in range(size(grid).y) for x in range(size(grid).x)
                   if grid[y][x] != '#' and len(neighbours(grid, intersection := Point(x, y), slippery)) > 2}.union({start, end})
  graph = {}
  for intersection in intersections: # for each intersection
    
    graph[intersection] = []
    for curr in neighbours(grid, intersection, slippery): # for each direction in intersection
      prev = intersection
      length = 1 # already 1 square away from intersection
      while True:
        if curr in intersections: # we have reached the other intersection
          graph[intersection].append((curr, length))
          break
        elif len(forward := (neighbours(grid, curr, slippery) - {prev})) < 1: # dead end or uphill slope
          break
        else: # move forward on trail section
          (prev, curr) = (curr, next(iter(forward)))
          length += 1
  
  return graph

def neighbours(grid, p, slippery):
  """Returns immediate valid neighbours (i.e. non-forest squares, cannot climb uphill if slope is slippery)"""
  return ({p + slope_directions[char]} if slippery and (char := grid[p.y][p.x]) in slope_directions.keys()
            else {n for delta in possible_directions
              if 0 <= (n := p + delta).x < size(grid).x
              and 0 <= n.y < size(grid).y
              and grid[n.y][n.x] != '#'})

def part(num):
  (start, end) = (Point(1, 0), Point(size(input_grid).x - 2, size(input_grid).y - 1))
  
  connections_graph = build_graph(input_grid, start, end, num == 1)
  
  max_length = 0
  to_visit = [([start],0)]
  
  while to_visit:
    (path, path_length) = to_visit.pop()
    curr = path[-1]
    
    if curr != end: # continue search with unvisited connections
      to_visit.extend((path + [connection], path_length + segment_length)
                      for connection, segment_length in connections_graph[curr] if connection not in path)
    elif path_length > max_length: # end of search, check and set record
      max_length = path_length

  return (max_length)

print(part(1))
print(part(2))