from Coordinates import Point, Polygon2D
from itertools import accumulate

input_lines = [line.split() for line in open('18_input.txt', 'r').read().splitlines()]

directions = {
    'R': Point(1, 0),
    'D': Point(0, 1),
    'L': Point(-1, 0),
    'U': Point(0, -1)
}

def part(num):
    dig_plan = [(dir, int(dist)) if num == 1 else
                (list(directions.keys())[int(color[-2])], int(color[2:-2], 16))
                for dir, dist, color in input_lines]

    polygon = Polygon2D(list(accumulate((directions[dir] * dist for (dir, dist) in dig_plan), initial = Point(0, 0))))

    return int(polygon.area() + polygon.perimeter() / 2 + 1)

print(part(1))
print(part(2))