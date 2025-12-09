from adventofcode2023.coordinates import Point, Rectangle
from adventofcode2023.helpers import init_2d

input_file = open('16_input.txt', 'r')
input_grid = [list(*line.split()) for line in input_file.read().splitlines()]

Direction = {
  'UP': Point(0,-1),
  'RIGHT': Point(1,0),
  'DOWN': Point(0,1),
  'LEFT': Point(-1,0)}

def forward_moves(direction, char):
  match char:
    case '.':
      return (direction,) # keep going
    case '\\':
      return (direction.flip_2d(),)
    case '/':
      return (direction.flip_2d().reverse(),)
    case '|':
      return ((direction,) if direction.x == 0 else
        (Direction['UP'], Direction['DOWN']))
    case '-':
      return ((direction,) if direction.y == 0 else
        (Direction['LEFT'], Direction['RIGHT']))

def beam_energy(grid, start_position, start_direction):
  (width, height) = (len(grid[0]), len(grid))
  grid_boundary = Rectangle(Point(0, 0), Point(width, height))

  visits = init_2d(height, width, set())
  to_visit = [(start_position, start_direction)]

  while to_visit:
    (position, direction) = to_visit.pop()
    
    if direction not in visits[position.y][position.x]: # ignore previous beams that have come through in the same way
      visits[position.y][position.x].add(direction)
      to_visit.extend((next_pos, next_dir) for next_dir in forward_moves(direction, grid[position.y][position.x])
                  if (next_pos := position + next_dir) in grid_boundary)
  
  return sum(bool(t) for row in visits for t in row)

def part_1():
  return beam_energy(input_grid, Point(0, 0), Direction['RIGHT'])

def part_2():
  (width, height) = (len(input_grid[0]), len(input_grid))

  edges = [[(Point(x, 0), Direction['DOWN']) for x in range(width)],
          [(Point(x, height - 1), Direction['UP']) for x in range(width)],
          [(Point(0, y), Direction['RIGHT']) for y in range(height)],
          [(Point(width - 1, y), Direction['LEFT']) for y in range(height)]]

  return max(beam_energy(input_grid, *start) for edge in edges for start in edge)

print(part_1())
print(part_2())