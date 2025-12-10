import itertools
from Coordinates import Point, Rectangle
from Helpers import consecutive_pairs

def solve(input_data):
    red_tiles = [Point.from_tuple(int(n) for n in line.split(',')) for line in input_data.splitlines()]

    return part_1(red_tiles), part_2(red_tiles)

def part_1(red_tiles):
    return max(Rectangle(r1, r2).area() for r1, r2 in itertools.combinations(red_tiles, 2))

def part_2(red_tiles):
    paths = consecutive_pairs(red_tiles, True)

    return max(r.area() for r1, r2 in itertools.combinations(red_tiles, 2)
               if all(not (r := Rectangle(r1, r2)).intersects(Rectangle(p1, p2), 1) for p1, p2 in paths))