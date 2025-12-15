from Coordinates import Point, Direction2D, Rectangle
from Helpers import width, height

def solve(input_data):
    grid = [[int(c) for c in row] for row in input_data.splitlines()]
    boundary = Rectangle(Point(width(grid), height(grid)))
    p1_score, p2_score = 0, 0

    for x in range(width(grid)):
        for y in range(height(grid)):
            if grid[y][x] == 0:
                stack = [(Point(x, y), 0)]
                summits = set()

                while stack:
                    p, elevation = stack.pop()
                    
                    if grid[p.y][p.x] == 9:
                        summits.add(p)
                        p2_score += 1
                    else:
                        for direction in Direction2D:
                            neighbor = p + direction.value
                            if neighbor in boundary and grid[neighbor.y][neighbor.x] == elevation + 1:
                                stack.append((neighbor, elevation + 1))
                p1_score += len(summits)
    return p1_score, p2_score