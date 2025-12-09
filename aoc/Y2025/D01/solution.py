def solve(input_data):
    moves = [int(line[1:]) * (-1 if line[0] == 'L' else 1) for line in input_data.splitlines()]
    return part_1(moves), part_2(moves)

def part_1(moves):
    #return sum([50] + moves) % 100
    dial = 50
    count = 0
    for move in moves:
        if dial == 0:
            count += 1
        dial = (dial + move) % 100
    return count

def part_2(moves):
    dial = 50
    count = 0
    for move in moves:
        count += abs(move) // 100
        if dial != 0 and not 0 < dial + (move % 100 if move > 0 else move % -100) < 100:
            count += 1
        dial = (dial + move) % 100
    return count