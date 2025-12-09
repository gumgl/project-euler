from collections import defaultdict
import itertools

input_lines = open('08_input.txt', 'r').read().splitlines()

grid = defaultdict(list)
for y, line in enumerate(input_lines):
    for x, char in enumerate(line):
        if char != '.':
            grid[char].append((x, y))

add = lambda a, b: (a[0] + b[0], a[1] + b[1])
mul = lambda a, n: (a[0] * n, a[1] * n)

def count_antinodes(max_distance):
    return len({antinode
        for locations in grid.values()
        for loc1, loc2 in itertools.permutations(locations, 2)
        for d in range(max_distance + 1)
        if 0 <= (antinode := add(mul(add(loc2, mul(loc1, -1)), d), loc2))[0] < len(input_lines[0]) and 0 <= antinode[1] < len(input_lines)
        })

print(count_antinodes(1))
print(count_antinodes(max(len(input_lines), len(input_lines[0]))))