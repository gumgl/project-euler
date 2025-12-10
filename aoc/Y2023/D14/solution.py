import copy
from enum import Enum
from Coordinates import Point

Direction = Enum('Direction', ['North', 'West', 'South', 'East'])

def solve(input_data):
    input_grid = [list(*line.split()) for line in input_data.splitlines()]
    
    return part(input_grid, 1), part(input_grid, 2)

def part(input_grid, num):
    grid = copy.deepcopy(input_grid)

    match num:
        case 1:
            roll(grid, Direction.North)
        case 2:
            spin(grid, 1000000000)
  
    return grid_score(grid)

def roll_line(grid, line_coords):
    """Given the coordinates contained in a row or column, roll the rocks to the start"""
    cube_indices = [i for i, point in enumerate(line_coords) if grid[point.y][point.x] == '#']
  
    if len(cube_indices) == 0 or cube_indices[0] != 0: # Prepend with a # to roll the first batch
        cube_indices = [-1] + cube_indices
  
    for cube_index in range(len(cube_indices)):
        line_start = cube_indices[cube_index] + 1
        line_end = cube_indices[cube_index + 1] if cube_index + 1 < len(cube_indices) else len(line_coords)
        rocks_count = sum(grid[point.y][point.x] == 'O' for point in line_coords[line_start : line_end])
        
        for coord in line_coords[line_start : line_start + rocks_count]:
            grid[coord.y][coord.x] = 'O'
        for coord in line_coords[line_start + rocks_count : line_end]:
            grid[coord.y][coord.x] = '.'

def roll(grid, direction):
    (width, height) = (len(grid[0]), len(grid))

    list_of_lines = [[]]
    match direction:
        case Direction.West:
            list_of_lines = [[Point(x, y) for x in range(width)] for y in range(height)]
        case Direction.East:
            list_of_lines = [[Point(x, y) for x in reversed(range(width))] for y in range(height)]
        case Direction.North:
            list_of_lines = [[Point(x, y) for y in range(height)] for x in range(width)]
        case Direction.South:
            list_of_lines = [[Point(x, y) for y in reversed(range(height))] for x in range(width)]
  
    for line in list_of_lines:
        roll_line(grid, line)

def spin_cycle(grid):
    for direction in [Direction.North, Direction.West, Direction.South, Direction.East]:
        roll(grid, direction)

def spin(grid, count):
    grid_history = []
    i = 0
    while i < count:
        grid_history.append(copy.deepcopy(grid))
        spin_cycle(grid)
        i += 1
        if grid in grid_history: # cycle found
            cycle_start = grid_history.index(grid)
            cycle_length = i - cycle_start
            remaining_cycles = count - i
            i = count - (remaining_cycles % cycle_length)

def grid_score(grid):
    (width, height) = (len(grid[0]), len(grid))
    return sum(len(grid) - y for x in range(width) for y in range(height) if grid[y][x] == 'O')