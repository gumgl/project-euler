from collections import Counter
from adventofcode2023.helpers import keys_by_value

input_file = open('13_input.txt', 'r')

input_grids = [[[*line.strip()] for line in input_grid.splitlines()] for input_grid in input_file.read().split('\n\n')]

def find_symmetries(haystack):
  """Returns k indices [i_k] where there are axes of symmetry
     i.e. haystack[0:i_k] == reverse(haystack[i_k:]) where 0 < i_k < len(haystack)-1"""
  h = list(haystack)
  return tuple(i for i,_ in enumerate(h) if 0 < i < len(haystack) and h[max(0,i-(len(h)-i)):i] == list(reversed(h[i:2*i])))

def find_vertical_symmetry(grid, look_for_smudge):
  """Returns first column index where there is a symmetry or smudge, 0 if none"""
  target_count = len(grid) - 1 if look_for_smudge else len(grid)

  row_symmetries = (list(find_symmetries(row)) for row in grid)
  counts = Counter(sum(row_symmetries, []))
  return keys[0] if (keys := list(keys_by_value(counts, target_count))) else 0

# General strategy: Grid-level vertical symmetries happen when row-level symmetries align (set intersection).
# For smudges, we look for symmetries aligned across all rows except one.
# For horizontal symmetry, we simply transpose the grid (flip along diagonal / swap rows and columns).
def part(num):
  return sum(find_vertical_symmetry(grid, num == 2) or 100 * find_vertical_symmetry(list(zip(*grid)), num == 2) for grid in input_grids)

print(part(1))
print(part(2))