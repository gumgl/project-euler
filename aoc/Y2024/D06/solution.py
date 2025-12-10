from Coordinates import Point, Rectangle
from Helpers import width, height

dir_vectors = [Point(0, -1), Point(1, 0), Point(0, 1), Point(-1, 0)]  # up, right, down, left

def solve(input_data):
    grid = input_data.splitlines()
    start = [Point(x, y) for y in range(height(grid)) for x in range(width(grid)) if grid[y][x] == '^'][0]
    
    # Part 1

    visited = [[False for x in range(width(grid))] for y in range(height(grid))]

    def mark_as_visited(pos, _):
        visited[pos.y][pos.x] = True
        return False

    traverse(grid, start, mark_as_visited)

    answers = [(sum(visited[y][x] for y in range(height(grid)) for x in range(width(grid))))]

    # Part 2

    modified_grid = lambda position: [['#' if (x, y) == position else grid[y][x] for x in range(width(grid))] for y in range(height(grid))]
    answers.append(sum(is_in_loop(modified_grid((x, y)), start) 
                       for y in range(height(grid)) for x in range(width(grid))
                       if visited[y][x] and Point(x, y) != start))
    return answers

def traverse(grid, start, visited_callback):
    """
    :param visited_callback: Function called at each visited position with (pos, direction). If it returns True, traversal stops.
    
    Returns:
        True if completed traversal, False if callback signaled to stop
    """
    guard_pos = start
    direction = 0  # start facing up
    while guard_pos in Rectangle(Point(width(grid), height(grid))):
        if grid[guard_pos.y][guard_pos.x] == '#':
            guard_pos = guard_pos - dir_vectors[direction] # step back
            direction = (direction + 1) % 4  # turn right
        else:
            if visited_callback(guard_pos, direction):
                return False
            guard_pos = guard_pos + dir_vectors[direction]  # move forward
    return True

def is_in_loop(grid, start):
    visited_directions = [[set() for _ in range(width(grid))] for _ in range(height(grid))]

    def callback(pos, direction):
        if direction in visited_directions[pos.y][pos.x]:
            return True # identical state previously seen
        visited_directions[pos.y][pos.x].add(direction)
        return False
    
    return not traverse(grid, start, callback)