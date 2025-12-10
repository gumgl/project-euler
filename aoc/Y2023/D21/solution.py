from Coordinates import Point
from Helpers import init_2d, consecutive_pairs

dirs = [Point(0,-1), Point(1,0), Point(0,1), Point(-1,0)]

def solve(input_data):
    input_grid = [list(*line.split()) for line in input_data.splitlines()]
    input_start = next(Point(x, y) for y, line in enumerate(input_grid) for x, c in enumerate(line) if c == 'S')
    input_grid[input_start.y][input_start.x] = '.' # to enable infinite grids

    return (part_1(input_grid, input_start), part_2(input_grid, input_start))

def count_garden_plots(grid, starts, max_distance, infinite_grid = False, return_distance_map = False):
    """Returns the number of garden plots visited in the form (even_count, odd_count) or the distance map"""
    (width, height) = (len(grid[0]), len(grid))
    boundary = set(starts) # BFS from multiple starts
    visited = set()
    distance = 0
    if return_distance_map:
        distance_map = init_2d(height, width, 0)
    else:
        counts = [0, 0]

    while boundary and distance <= max_distance:
        next_boundary = set()
        for tile in boundary:
            if tile not in visited:
                visited.add(tile)
                if return_distance_map:
                    distance_map[tile.y][tile.x] = distance
                else:
                    counts[distance % 2] += 1
                
                next_boundary.update(n
                    for direction in dirs
                    if (n := tile + direction) not in visited
                    and (infinite_grid or 0 <= n.y < height and 0 <= n.x < width)
                    and grid[n.y % height][n.x % width] == '.')
        distance += 1
        boundary = next_boundary
    #[print(*['█' if c == '#' else ' ' for c in line], sep='') for line in grid]
    return distance_map if return_distance_map else counts

def part_1(input_grid, input_start):
    return count_garden_plots(input_grid, [input_start], n := 64)[n % 2]

def part_2(input_grid, input_start):
    """Strategy: Break the search down into nested diamonds, then algebraically derive a closed-form formula in
    terms of the number of nested diamonds. Note we double the thickness of diamond layers to handle even-odd parity.
    See 21_diamond_grid.svg where c0 ⊆ c1 ⊆ c2. Total_count = purple_center + x * green_strips + 4x^2 * blue_squares
    """
    def verify_assumptions():
        # We require a square grid
        assert len(input_grid) == len(input_grid[0])

        # We require the layout in the grid to be such that the search expands uniformally in both dimensions
        # in the shape of a diamond and that the garden plot counts follow a quadratic relationship in terms of the layer depth
        distances = count_garden_plots(input_grid, [input_start], size // 2, return_distance_map=True)
        assert all(distances[size // 2 + d.y * i][size // 2 + d.x * (size // 2 - i)] == distances[0][size // 2]
                for i in range(size // 2)
                for d in [d1 + d2 for d1, d2 in consecutive_pairs(dirs)])
        
        # We require a whole number of nested double-thickness diamonds around the initial center diamond
        assert (target_distance - size // 2) % (size * 2) == 0

    target_distance = 26501365
    size = len(input_grid)
    verify_assumptions()

    # Counts for center diamond and first two outer diamonds
    c = [count_garden_plots(input_grid, [input_start], size // 2 + size * 2 * x, infinite_grid=True)[target_distance % 2]
        for x in range(3)]

    x = (target_distance - size // 2) // (size * 2) # number of nested diamonds
    return c[0] + x * (4 * c[1] - 3 * c[0] - c[2]) // 2 + (x ** 2) * ((c[2] - 2 * c[1] + c[0]) // 2)