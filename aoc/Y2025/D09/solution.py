import itertools

def solve(input_data):
    red_tiles = [tuple(int(n) for n in line.split(',')) for line in input_data.splitlines()]
    return (part_1(red_tiles), part_2(red_tiles))

rectangle_area = lambda p1, p2: (abs(p2[0] - p1[0]) + 1) * (abs(p2[1] - p1[1]) + 1)

def part_1(red_tiles):
    return max(rectangle_area(r1, r2) for r1, r2 in itertools.combinations(red_tiles, 2))

def part_2(red_tiles):
    def intersecting(r1, r2, p1, p2):
        return not (max(r1[0], r2[0]) <= min(p1[0], p2[0]) or 
                    min(r1[0], r2[0]) >= max(p1[0], p2[0]) or
                    max(r1[1], r2[1]) <= min(p1[1], p2[1]) or
                    min(r1[1], r2[1]) >= max(p1[1], p2[1]))
    paths = list(itertools.pairwise(red_tiles)) + [(red_tiles[-1], red_tiles[0])]

    return max(rectangle_area(r1, r2) for r1, r2 in itertools.combinations(red_tiles, 2)
               if all(not intersecting(r1, r2, p1, p2) for p1, p2 in paths))