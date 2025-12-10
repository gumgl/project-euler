from Coordinates import Point, Polygon2D
from itertools import accumulate

directions = {
    'R': Point(1, 0),
    'D': Point(0, 1),
    'L': Point(-1, 0),
    'U': Point(0, -1)
}

def solve(input_data):
    input_lines = [line.split() for line in input_data.splitlines()]

    return part(input_lines, 1), part(input_lines, 2)

def part(input_lines, num):
    dig_plan = [(dir, int(dist)) if num == 1 else
                (list(directions.keys())[int(color[-2])], int(color[2:-2], 16))
                for dir, dist, color in input_lines]

    polygon = Polygon2D(list(accumulate((directions[dir] * dist for (dir, dist) in dig_plan), initial = Point(0, 0))))

    return int(polygon.area() + polygon.perimeter() / 2 + 1)