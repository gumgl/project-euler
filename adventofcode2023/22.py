from adventofcode2023.coordinates import Point
from adventofcode2023.helpers import init_2d

class Brick:
  def __init__(self, start, end):
    self.start = start
    self.end = end
    self.stable = False
  
  def __add__(self, delta):
    return Brick(self.start + delta, self.end + delta)
  
  def move(self, delta):
    self.start = self.start + delta
    self.end = self.end + delta
  
  def moveToZ(self, z):
    (self.start.z, self.end.z) = (z, z + self.end.z - self.start.z)

  def __str__(self):
    return f"{self.start}~{self.end}"

  def intersects(self, other):
    return not (self.end.x < other.start.x
          or other.end.x < self.start.x
          or self.end.y < other.start.y
          or other.end.y < self.start.y
          or self.end.z < other.start.z
          or other.end.z < self.start.z)

input_file = open('22_input.txt', 'r')

input_bricks = [Brick(*map(lambda c: Point(*eval(c)), line.split('~'))) for line in input_file.read().splitlines()]

# no diagonals
assert all(sum((brick.start.x != brick.end.x, brick.start.y != brick.end.y, brick.start.z != brick.end.z)) <= 1 for brick in input_bricks)

# order of start and end
assert all(brick.start.x <= brick.end.x and brick.start.y <= brick.end.y and brick.start.z <= brick.end.z for brick in input_bricks)

# no negatives coordinates
assert all(brick.start.x >= 0 and brick.start.y >= 0 and brick.start.z >= 0 for brick in input_bricks)

def settle(bricks_list):
  bricks = sorted(bricks_list, key=lambda brick: brick.start.z)
  
  top_brick_index = init_2d(max(b.end.y for b in bricks) + 1, max(b.end.x for b in bricks) + 1, -1)

  supported_by = [set() for _ in range(len(bricks))]
  supports = [set() for _ in range(len(bricks))]

  for i, brick in enumerate(bricks): # drop in place
    # x,y-coordinates of area covered by brick
    xys = [(x, y) for x in range(brick.start.x, brick.end.x + 1) for y in range(brick.start.y, brick.end.y + 1)]

    # move to highest obstacle or ground
    brick.moveToZ(max([0] + [bricks[top_index].end.z for x, y in xys if (top_index := top_brick_index[y][x]) >= 0]) + 1)

    # keep track of which blocks act as support (collisions)
    supported_by[i] = {top_index for x, y in xys if (top_index := top_brick_index[y][x]) >= 0 and bricks[top_index].end.z == (brick.start.z - 1)}
    for j in supported_by[i]:
      supports[j].add(i)
    
    # keep track of which block is at the top
    for x in range(brick.start.x, brick.end.x + 1):
      for y in range(brick.start.y, brick.end.y + 1):
        top_brick_index[y][x] = i
  
  return bricks, supported_by, supports
  #side_view(bricks, False)
  #side_view(bricks, True)

def count_chain_reaction(i, supported_by, supports):
  fallen = set()
  will_fall = {i}

  while will_fall:
    fallen.update(will_fall) # make them fall
    # select the dependents of the next batch which are going to fall
    will_fall = {dependent for detached in will_fall for dependent in supports[detached]
                       if len(supported_by[dependent] - fallen) == 0}
  
  return len(fallen) - 1 # do not count i itself

def part_1():
  (bricks, supported_by, _) = settle(input_bricks)
  # count bricks who are not sole supports of other bricks
  return sum({i} not in supported_by for i in range(len(bricks)))

def part_2():
  
  (bricks, supported_by, supports) = settle(input_bricks)

  return sum(count_chain_reaction(i, supported_by, supports) for i in range(len(bricks)))

def side_view(bricks, x_view):
  def p(s):
    print(s, end='')

  max_xy = max(b.end.y for b in bricks) if x_view else max(b.end.x for b in bricks)
  max_z = max(b.end.z for b in bricks)
  print(' y' if x_view else ' x')
  for z in reversed(range(max_z + 2)):
    for xy in range(max_xy + 1):
      if z == 0:
        p('-')
      elif z > max_z:
        p(xy)
      else:
        bs = [i for i, b in enumerate(bricks) if b.start.z <= z <= b.end.z and
              (x_view and b.start.y <= xy <= b.end.y or
              not x_view and b.start.x <= xy <= b.end.x)]
        match len(bs):
          case 0: 
            p('.')
          case 1:
            p(chr(65 + bs[0]))
          case _:
            p('?')
    p(' ')
    if z <= max_z:
      p(z)
    if z == max_z // 2:
      p(' z')
    p('\n')

print(part_1())
print(part_2())