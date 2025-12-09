input_lines = open('04_input.txt', 'r').read().splitlines()

transposed = lambda matrix: [list(row) for row in zip(*matrix)]
rotated = lambda matrix: [list(reversed(col)) for col in zip(*matrix)]
flipped_horizontally = lambda matrix: [list(reversed(row)) for row in matrix]
flipped_vertically = lambda matrix: list(reversed(matrix))

def diagonals(grid):
    x_max = len(grid[0])
    y_max = len(grid)
    return (
        [[grid[i][x+i] for i in range(x_max - x)] for x in range(x_max)] +
        [[grid[y+i][i] for i in range(y_max - y)] for y in range(1, y_max)]
        
    )

def part_1():
    searches = sum([
        input_lines,
        transposed(input_lines),
        diagonals(input_lines),
        diagonals(flipped_horizontally(input_lines))
        ],[])

    return sum(''.join(line).count('XMAS') + ''.join(reversed(line)).count('XMAS') for line in searches)

is_xmas = lambda grid: ''.join([grid[0][0], grid[2][0], grid[1][1], grid[0][2], grid[2][2]]) == 'MMASS'

def part_2():
    return sum(is_xmas(grid := [line[x:x+3] for line in input_lines[y:y+3]])
               + is_xmas(flipped_horizontally(grid))
               + is_xmas(transposed(grid))
               + is_xmas(flipped_horizontally(transposed(grid)))
               for x in range(len(input_lines[0])-2) for y in range(len(input_lines)-2))

print(part_1())
print(part_2())