from Coordinates import Point, Cube
from heapq import heappop, heappush

input_grid = [[int(x) for x in line] for line in open('17_input.txt', 'r').read().splitlines()]

directions = {Point(-1, 0), Point(0, -1), Point(1, 0), Point(0, 1)}

def shortest_path(weights, source, target, min_straight, max_straight):
  """The solution involves building a graph whose nodes are defined by coordinates and the
  direction that is not allowed to continue, and edges are the legal straight moves to the left and right.
  The edges are computed dynamically and added to the queue as we search the graph."""
  (width, height) = (len(weights[0]), len(weights))
  max_distance = width * height * 9
  grid_boundary = Cube(Point(0, 0), Point(width - 1, height - 1))

  queue = [(0, source, Point(0, 0))] # priority queue for Dijkstra's algorithm. note: tuples compare lexicographically.
  seen = set() # to avoid revisiting the same (node, direction) twice
  dist = {} # to quickly retrieve the best cost per (node, direction)

  while queue:
    (u_distance, u, u_direction) = heappop(queue)
    if u == target:
      return u_distance
    
    if (u, u_direction) not in seen:
      seen.add((u, u_direction))

      for v_direction in directions - {u_direction, u_direction.reverse()}:
        v_distance = u_distance
        for i in range(1, max_straight + 1):
          if (v := u + i * v_direction) in grid_boundary:
            v_distance += weights[v.y][v.x] # accumulate distances before being able to turn
            if i >= min_straight and v_distance < dist.get((v, v_direction), max_distance):
              # If we did not find a worse path than before
              dist[(v, v_direction)] = v_distance
              heappush(queue, (v_distance, v, v_direction))

def part(num):
  straights = ((0, 3), (4, 10))
  return shortest_path(input_grid, Point(0, 0), Point(len(input_grid[0]) - 1, len(input_grid) - 1), *straights[num-1])

print(part(1))
print(part(2))