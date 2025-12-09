input_lines = open('06_input.txt', 'r').read().splitlines()
width = len(input_lines[0])
height = len(input_lines)
dir_vectors = [(0, -1), (1, 0), (0, 1), (-1, 0)]  # up, right, down, left

start = [(x, y) for y in range(height) for x in range(width) if input_lines[y][x] == '^'][0]
add = lambda a, b: (a[0] + b[0], a[1] + b[1])
mul = lambda a, n: (a[0] * n, a[1] * n)

def traverse(grid, visited_callback):
    """
    :param visited_callback: Function called at each visited position with (pos, direction). If it returns True, traversal stops.
    
    Returns:
        True if completed traversal, False if callback signaled to stop
    """
    guard_pos = start
    direction = 0  # start facing up
    while 0 <= guard_pos[0] < width and 0 <= guard_pos[1] < height:
        if grid[guard_pos[1]][guard_pos[0]] == '#':
            guard_pos = add(guard_pos, mul(dir_vectors[direction], -1)) # step back
            direction = (direction + 1) % 4  # turn right
        else:
            if visited_callback(guard_pos, direction):
                return False
            guard_pos = add(guard_pos, dir_vectors[direction])  # move forward
    return True

def is_in_loop(grid):
    visited_directions = [[set() for _ in range(width)] for _ in range(height)]

    def callback(pos, direction):
        if direction in visited_directions[pos[1]][pos[0]]:
            return True # identical state previously seen
        visited_directions[pos[1]][pos[0]].add(direction)
        return False
    
    return not traverse(grid, callback)

# Part 1

visited = [[False for x in range(width)] for y in range(height)]

def mark_as_visited(pos, direction):
    visited[pos[1]][pos[0]] = True
    return False

traverse(input_lines, mark_as_visited)

print(sum(visited[y][x] for y in range(height) for x in range(width)))

# Part 2

modified_grid = lambda position: [['#' if (x, y) == position else input_lines[y][x] for x in range(width)] for y in range(height)]

print(sum(is_in_loop(modified_grid((x, y)))for y in range(height) for x in range(width) if visited[y][x] and (x,y) != start))