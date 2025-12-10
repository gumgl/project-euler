from collections import defaultdict
import itertools
from Coordinates import Point, Rectangle

def solve(input_data):
    input_lines = input_data.splitlines()
    grid = defaultdict(list)

    for y, line in enumerate(input_lines):
        for x, char in enumerate(line):
            if char != '.':
                grid[char].append(Point(x, y))

    def count_antinodes(distances):
        return len({antinode
            for antenna_type_locations in grid.values()
            for loc1, loc2 in itertools.permutations(antenna_type_locations, 2)
            for d in distances
            if (antinode := ((loc2 - loc1) * d + loc2)) in Rectangle(Point(len(input_lines[0]), len(input_lines)))
            })

    return (count_antinodes([1]),
            count_antinodes(range(max(len(input_lines), len(input_lines[0])))))