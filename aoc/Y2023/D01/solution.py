import re

input_lines = open('01_input.txt', 'r').read().splitlines()

def add_first_and_last(lines):
    return sum(line[0] * 10 + line[-1] for line in lines)

def part_1():
    digits = [[int(x) for x in re.findall(r'\d', line)] for line in input_lines]
    return add_first_and_last(digits)

def part_2():
    options = (r'\d', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine')
    return add_first_and_last(
        [int(match) if len(match) == 1 else options.index(match)
            # Using lookahead to cover overlapping matches e.g. in the case of line ending in 'eightwo'
            for match in re.findall(f"(?=({'|'.join(options)}))", line)]
        for line in input_lines)

print(part_1())
print(part_2())
