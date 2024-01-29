from Coordinates import Point, Cube
from Helpers import pairs
import copy
import z3

input_lines = open('24_input.txt', 'r').read().splitlines()
projectiles = [(Point(*eval((s := line.split(' @ '))[0])), Point(*eval(s[1]))) for line in input_lines]

def reduce_box(proj1, proj2, start_box):
    # Adjust target boundary based on lines' directions and starting points
    (s, d) = zip(proj1, proj2)
    reduced_box = copy.deepcopy(start_box)

    for xy in 'xy':
        for i in range(2):
            if d[i][xy] >= 0:
                reduced_box.lower[xy] = max(reduced_box.lower[xy], s[i][xy])
            if d[i][xy] <= 0:
                reduced_box.upper[xy] = min(reduced_box.upper[xy], s[i][xy])

    return reduced_box

def xy_intersection(proj1, proj2):
    (s1, d1) = proj1
    (s2, d2) = proj2

    if d1.x == 0 and d2.x == 0: # two vertical lines
        return s1 if s1.x == s2.x else None
    elif d1.x == 0: # only one vertical line
        y = (s1.x - s2.x) * d2.y / d2.x + s2.y # derive y along proj2 at s1.x
        #x = (y - s2.y) * d2.x / d2.y + s2.x # derive x along proj2 at y
        x = s1.x
        return Point(x, y)
    elif d2.x == 0: # only one vertical line
        y = (s2.x - s1.x) * d1.y / d1.x + s1.y # derive y along proj1 at s2.x
        #x = (y - s1.y) * d1.x / d1.y + s1.x # derive x along proj1 at y
        x = s2.x
        return Point(x, y)
    elif d1.y * d2.x - d2.y * d1.x == 0: # parallel lines
        # simply plug s2 into equation for proj1
        return s1 if (s2.x - s1.x) * d1.y / d1.x == s2.y - s1.y else None # either they overlap fully or never
    else:
        x = (d1.x * d2.x / (d1.y * d2.x - d1.x * d2.y)) * (d1.y * s1.x / d1.x - d2.y * s2.x / d2.x + s2.y - s1.y)
        y1 = (d1.y / d1.x) * (x - s1.x) + s1.y
        y2 = (d2.y / d2.x) * (x - s2.x) + s2.y
        # t1 = (x - s1.x) / d1.x
        # t2 = (x - s2.x) / d2.x
        return Point(x, y1)

def part_1():
    search_box = Cube(Point(1, 1) * 200000000000000, Point(1, 1) * 400000000000000)

    return sum(
        (b := reduce_box(p1, p2, search_box)).area() > 0
        and (p := xy_intersection(p1, p2)) is not None 
        and p in b
        for p1, p2 in pairs(projectiles))

def part_2():
    T = [z3.Int(f't{i}') for i in range(3)] # one `t` for each of the first 3 projectiles
    (S, D) = ({dim: z3.Int(f'{v}{dim}') for dim in 'xyz'} for v in 'sd') # 3 dimensions of s and d of target
    
    s = z3.Solver()
    for i in range(3):
        for dim in 'xyz':
            s.add(S[dim] + T[i] * D[dim] == projectiles[i][0][dim] + T[i] * projectiles[i][1][dim])
    s.check()

    return s.model().eval(sum(S.values()))

print(part_1())
print(part_2())