import re

def part_1(instructions):
    return sum(int(inst[1]) * int(inst[2]) for inst in instructions if inst[0] == 'mul')

def part_2(instructions):
    result = 0
    enabled = True
    for inst in instructions:
        match inst[0]:
            case "mul":
                if enabled:
                    result += int(inst[1]) * int(inst[2])
            case "do":
                enabled = True
            case "don't":
                enabled = False
    return result

def solve(input_data):
    instructions = re.findall(r'(mul|do|don\'t)\((?:(\d+),(\d+))?\)', input_data)
    return part_1(instructions), part_2(instructions)