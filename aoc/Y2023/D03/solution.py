from math import prod
import re
from Coordinates import Rectangle, Point

def solve(input_data):
    input_grid = [list(*line.split()) for line in input_data.splitlines()]
    # [(part number, row #, col # start, col # end)]
    potential_parts = [(int(m.group(0)), i, *m.span())
                       for i, line in enumerate(input_data.splitlines())
                       for m in re.finditer(r'\d+', line)]
    return (part_1(input_grid, potential_parts),
            part_2(input_grid, potential_parts))
    
def part_1(grid, potential_parts):
    grid_boundary = Rectangle(Point(len(grid[0]) - 1, len(grid) - 1))
    def is_symbol(c):
        return c not in '.0123456789'
    
    def is_real_part(part):
        (_, row, start, end) = part
        return any(is_symbol(grid[p.y][p.x])
            for p in grid_boundary.overlap_2d(Rectangle(Point(start - 1, row - 1), Point(end + 1, row + 2))).lattice_points_2d())

    return sum(part[0] for part in potential_parts if is_real_part(part))

def part_2(grid, potential_parts):
    def neighbour_parts(gear_point):
        return {number
                for (number, row, start, end) in potential_parts
                if gear_point in Rectangle(Point(start - 1, row - 1), Point(end + 1, row + 2))}
    
    potential_gears = [neighbour_parts(Point(x, y))
                for y, row in enumerate(grid) for x, c in enumerate(row)
                if c == '*']
    
    return sum(prod(parts) for parts in potential_gears if len(parts) == 2)