input_grid = open('04_input.txt', 'r').read().splitlines()

grid_width = len(input_grid[0])
grid_height = len(input_grid)

def is_accessible(grid, x, y):
    return grid[y][x] == '@' and sum(not (x == xp and y == yp) and grid[yp][xp] == '@' for xp in range(max(0, x-1), min(grid_width, x+2)) for yp in range(max(0, y-1), min(grid_height, y+2))) < 4

def count_accessible(grid):
    return sum(is_accessible(grid, x, y) for x in range(grid_width) for y in range(grid_height))

def part_1():
    return count_accessible(input_grid)

def part_2():
    grid = [list(line) for line in input_grid]
    
    count = 0

    while True:
        newcount = count_accessible(grid)
        if newcount == 0:
            break
        count += newcount
        
        grid = [['.' if is_accessible(grid, x, y) else grid[y][x] for x in range(grid_width)] for y in range(grid_height)]

    return count

print(part_1())
print(part_2())