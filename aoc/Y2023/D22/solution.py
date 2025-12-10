from typing import List
from Coordinates import Point, RectangularCuboid
from Helpers import init_2d

def move_brick_to_z(r: RectangularCuboid, z: int):
     (r.lower.z, r.upper.z) = (z, z + r.upper.z - r.lower.z)

def solve(input_data):
    input_bricks = [RectangularCuboid(*map(lambda c: Point(*eval(c)), line.split('~'))) for line in input_data.splitlines()]

    # no diagonals
    assert all(sum((brick.lower.x != brick.upper.x, brick.lower.y != brick.upper.y, brick.lower.z != brick.upper.z)) <= 1 for brick in input_bricks)

    # order of start and end
    assert all(brick.lower.x <= brick.upper.x and brick.lower.y <= brick.upper.y and brick.lower.z <= brick.upper.z for brick in input_bricks)

    # no negatives coordinates
    assert all(brick.lower.x >= 0 and brick.lower.y >= 0 and brick.lower.z >= 0 for brick in input_bricks)

    return part_1(input_bricks), part_2(input_bricks)

def settle(bricks_list : list[RectangularCuboid]):
    bricks = sorted(bricks_list, key=lambda brick: brick.lower.z)
    
    top_brick_index = init_2d(max(b.upper.y for b in bricks) + 1, max(b.upper.x for b in bricks) + 1, -1)

    supported_by = [set() for _ in range(len(bricks))]
    supports = [set() for _ in range(len(bricks))]

    for i, brick in enumerate(bricks): # drop in place
        # x,y-coordinates of area covered by brick
        xys = [(x, y) for x in range(brick.lower.x, brick.upper.x + 1) for y in range(brick.lower.y, brick.upper.y + 1)]
        xyz = brick.lattice_points_2d(padding_upper = Point.unit(3))

        # move to highest obstacle or ground
        move_brick_to_z(brick, max([0] + [bricks[top_index].upper.z for x, y in xys if (top_index := top_brick_index[y][x]) >= 0]) + 1)

        # keep track of which blocks act as support (collisions)
        supported_by[i] = {top_index for x, y in xys if (top_index := top_brick_index[y][x]) >= 0 and bricks[top_index].upper.z == (brick.lower.z - 1)}
        for j in supported_by[i]:
            supports[j].add(i)
        
        # keep track of which block is at the top
        for p in brick.lattice_points_2d(padding_upper = Point.unit(3)):
                top_brick_index[p.y][p.x] = i
  
    return bricks, supported_by, supports
    #side_view(bricks, False)
    #side_view(bricks, True)

def count_chain_reaction(i, supported_by, supports):
    fallen = set()
    will_fall = {i}

    while will_fall:
        fallen.update(will_fall) # make them fall
        # select the dependents of the next batch which are going to fall
        will_fall = {dependent for detached in will_fall for dependent in supports[detached]
                        if len(supported_by[dependent] - fallen) == 0}
  
    return len(fallen) - 1 # do not count i itself

def part_1(input_bricks):
    (bricks, supported_by, _) = settle(input_bricks)
    # count bricks who are not sole supports of other bricks
    return sum({i} not in supported_by for i in range(len(bricks)))

def part_2(input_bricks):
    (bricks, supported_by, supports) = settle(input_bricks)

    return sum(count_chain_reaction(i, supported_by, supports) for i in range(len(bricks)))

def side_view(bricks : List[RectangularCuboid], x_view):
    def p(s):
        print(s, end='')

    max_xy = max(b.upper.y for b in bricks) if x_view else max(b.upper.x for b in bricks)
    max_z = max(b.upper.z for b in bricks)
    print(' y' if x_view else ' x')
    for z in reversed(range(max_z + 2)):
        for xy in range(max_xy + 1):
            if z == 0:
                p('-')
            elif z > max_z:
                p(xy)
            else:
                bs = [i for i, b in enumerate(bricks) if b.lower.z <= z <= b.upper.z and
                    (x_view and b.upper.y <= xy <= b.upper.y or
                    not x_view and b.lower.x <= xy <= b.upper.x)]
                match len(bs):
                    case 0: 
                            p('.')
                    case 1:
                            p(chr(65 + bs[0]))
                    case _:
                            p('?')
        p(' ')
        if z <= max_z:
            p(z)
        if z == max_z // 2:
            p(' z')
        p('\n')