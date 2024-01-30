from math import prod
import re
from adventofcode2023.coordinates import Rectangle, Point

input_lines = open('03_input.txt', 'r').read().splitlines()
input_grid = [list(*line.split()) for line in input_lines]
# [(part number, row #, col # start, col # end)]
potential_parts = [(int(m.group(0)), i, *m.span()) for i, line in enumerate(input_lines) for m in re.finditer(r'\d+', line)]
grid_boundary = Rectangle(Point(0, 0), Point(len(input_grid[0]) - 1, len(input_grid) - 1))

def part_1():
    def is_symbol(c):
        return c not in '.0123456789'
    
    def is_real_part(part):
        (_, row, start, end) = part
        return any(is_symbol(input_grid[p.y][p.x])
            for p in grid_boundary.overlap(Rectangle(Point(start - 1, row - 1), Point(end + 1, row + 2))).lattice_points())

    return sum(part[0] for part in potential_parts if is_real_part(part))

def part_2():
    def neighbour_parts(gear_point):
        return {number
                for (number, row, start, end) in potential_parts
                if gear_point in Rectangle(Point(start - 1, row - 1), Point(end + 1, row + 2))}
    
    potential_gears = [neighbour_parts(Point(x, y))
                for y, row in enumerate(input_grid) for x, c in enumerate(row)
                if c == '*']
    
    return sum(prod(parts) for parts in potential_gears if len(parts) == 2)

print(part_1())
print(part_2())