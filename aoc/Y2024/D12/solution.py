from Coordinates import Point, Direction2D
from Helpers import width, height, pairwise_sum
"""
Strategy: Union-find regions while counting area, perimeter, and sides.
Area: simply count each cell in a region.
Perimeter: simply count adjacent cells of a different region (different character).
Sides: count a side only when it is not already connecting to a previous cell's same side in the same region.
"""
def solve(input_data):
    grid = input_data.splitlines()
    pgrid = {Point(x, y): grid[y][x] for x in range(width(grid)) for y in range(height(grid))}

    parent = {} # {all points on grid: root point for the region}
    region_counts = {} # {root point for each region: [area, perimeter, sides]}
    fences = {} # {all points on grid: list of directions that are fences}

    for y in range(height(grid)):
        for x in range(width(grid)):
            p = Point(x, y)
            c = pgrid[p]

            ancestors = [n for d in (Direction2D.UP, Direction2D.LEFT) if pgrid.get(n := p + d.value) == c]
            fences[p] = [d for d in Direction2D if pgrid.get(p + d.value) != c]
            perimeter_count = len(fences[p])

            # For left and right, look up. For up and down, look left.
            side_look_direction = lambda d: d.abs().flip_2d().reverse()

            # For each fence of current cell, only count it if it does not connect to a same-region cell on that side
            side_count = sum(pgrid.get(n := p + side_look_direction(d.value)) != c or d not in fences.get(n, [])
                             for d in fences[p])

            if len(ancestors) == 2 and parent[ancestors[0]] != parent[ancestors[1]]: # merge two regions
                # merge counts
                region_counts[parent[ancestors[0]]] = \
                    list(pairwise_sum(*(region_counts[parent[ancestor]] for ancestor in ancestors)))
                region_counts[parent[ancestors[1]]] = [0, 0, 0]
                
                # update parents
                for k,v in parent.items():
                    if v == parent[ancestors[1]]:
                        parent[k] = parent[ancestors[0]]
            
            if ancestor := next(iter(ancestors), None): # extend an ancestor region
                region_counts[parent[ancestor]][0] += 1
                region_counts[parent[ancestor]][1] += perimeter_count
                region_counts[parent[ancestor]][2] += side_count
                parent[p] = parent[ancestors[0]]
            else: # new region
                region_counts[p] = [1, perimeter_count, side_count]
                parent[p] = p

    return sum(area * perimeter for area, perimeter, _ in region_counts.values()), \
           sum(area * sides for area, _, sides in region_counts.values())