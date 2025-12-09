input_lines = open('01_input.txt', 'r').read().splitlines()
moves = [int(line[1:]) * (-1 if line[0] == 'L' else 1) for line in input_lines]

def part_1():
    #return sum([50] + moves) % 100
    dial = 50
    count = 0
    for move in moves:
        if dial == 0:
            count += 1
        dial = (dial + move) % 100
    return count

def part_2():
    dial = 50
    count = 0
    for move in moves:
        count += abs(move) // 100
        if dial != 0 and not 0 < dial + (move % 100 if move > 0 else move % -100) < 100:
            count += 1
        dial = (dial + move) % 100
    return count

print(part_1())
print(part_2())