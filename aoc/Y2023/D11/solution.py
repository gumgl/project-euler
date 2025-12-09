from adventofcode2023.coordinates import Point
from adventofcode2023.helpers import indices_2d, pairs

input_file = open('11_input.txt', 'r')

input_grid = [[*line.strip()] for line in input_file.readlines()]

def find_galaxies(grid):
  return (Point(*p) for p in indices_2d(grid, '#', True))

def empty_lines(grid):
  """Return indices of empty lines ([rows],[columns])"""
  return ((y for (y, line) in enumerate(grid) if all(c != '#' for c in line)),
          (x for (x,_) in enumerate(grid[0]) if all(line[x] != '#' for line in grid)))

def expand_galaxies(grid, galaxies, factor):
  """ Add `factor` to coordinates for each empty row/col that comes before each galaxy"""
  (rows,cols) = map(list, empty_lines(grid)) # lists for reuse
  # (factor - 1) because empty lines are already counted once
  return (Point(g.x + (factor - 1) * sum(x < g.x for x in cols),
                g.y + (factor - 1) * sum(y < g.y for y in rows)
                ) for g in galaxies)

def sum_distances(galaxies):
  return sum(g1.manhattan_distance(g2) for g1,g2 in pairs(galaxies))

def part(num):
  factors = [2, 10**6]
  return sum_distances(expand_galaxies(input_grid, find_galaxies(input_grid), factors[num-1]))

print(part(1))
print(part(2))