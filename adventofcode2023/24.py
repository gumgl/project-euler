from Coordinates import Point
from Helpers import pairs

input_lines = open('24_input.txt', 'r').read().splitlines()

coords = [(Point(*eval((s := line.split(' @ '))[0])), Point(*eval(s[1]))) for line in input_lines]

def compute_boundaries(proj1, proj2, min_bound, max_bound):
  # Adjust target boundary based on lines' directions and starting points

  (s1, d1) = proj1
  (s2, d2) = proj2

  if d1.x >= 0:
    min_bound.x = max(min_bound.x, s1.x)
  if d1.x <= 0:
    max_bound.x = min(max_bound.x, s1.x)

  if d2.x >= 0:
    min_bound.x = max(min_bound.x, s2.x)
  if d2.x <= 0:
    max_bound.x = min(max_bound.x, s2.x)

  if d1.y >= 0:
    min_bound.y = max(min_bound.y, s1.y)
  if d1.y <= 0:
    max_bound.y = min(max_bound.y, s1.y)

  if d2.y >= 0:
    min_bound.y = max(min_bound.y, s2.y)
  if d2.y <= 0:
    max_bound.y = min(max_bound.y, s2.y)

  return min_bound, max_bound

def intersect(proj1, proj2, min_bound, max_bound):
  (s1, d1) = proj1
  (s2, d2) = proj2

  min_bound, max_bound = compute_boundaries(proj1, proj2, min_bound, max_bound)

  if max_bound.x < min_bound.x or max_bound.y < min_bound.y: # either one line starts outside of boundaries
    return False #  or two lines are going in opposite directions in either axis

  # Note: two vertical lines (intersection = d1.x == 0 and d2.x == 0 and s1.x == s2.x) are covered by bounds checking
  if d1.x == 0: # only one vertical line
    y = (s1.x - s2.x) * d2.y / d2.x + s2.y # derive y along proj2 at s1.x
    x = (y - s2.y) * d2.x / d2.y + s2.x # derive x along proj2 at y
    print('proj1 vertical')
    return min_bound.x < x < max_bound.x and min_bound.y < y < max_bound.y
  elif d2.x == 0: # only one vertical line
    y = (s2.x - s1.x) * d1.y / d1.x + s1.y # derive y along proj1 at s2.x
    x = (y - s1.y) * d1.x / d1.y + s1.x # derive x along proj1 at y
    return min_bound.x < x < max_bound.x and min_bound.y < y < max_bound.y
  elif d1.y * d2.x - d2.y * d1.x == 0: # parallel lines
    # check whether they overlap fully or never (no bounds check)
    return (s2.x - s1.x) * d1.y / d1.x == s2.y - s1.y # simply plug s2 into equation for proj1
  else:
    x = (d1.x * d2.x / (d1.y * d2.x - d1.x * d2.y)) * (d1.y * s1.x / d1.x - d2.y * s2.x / d2.x + s2.y - s1.y)
    y1 = (d1.y / d1.x) * (x - s1.x) + s1.y
    y2 = (d2.y / d2.x) * (x - s2.x) + s2.y
    # t1 = (x - s1.x) / d1.x
    # t2 = (x - s2.x) / d2.x

    return min_bound.x < x < max_bound.x and min_bound.y < y1 < max_bound.y

def part_1():
  (lower, upper) = (200000000000000, 400000000000000)
  return sum(intersect(p1, p2, Point(lower, lower), Point(upper, upper)) for p1, p2 in pairs(coords))

print(part_1())