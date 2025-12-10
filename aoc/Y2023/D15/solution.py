from functools import reduce

def solve(input_data):
    commands = list(input_data.strip().split(','))

    return part_1(commands), part_2(commands)

def part_1(commands):
    return sum(hash(command) for command in commands)

def part_2(commands):
    boxes = [{} for _ in range(256)]

    for command in commands:
        if command[-2] == '=':
            label = command[:-2]
            boxes[hash(label)][label] = int(command[-1])
        else:
            label = command[:-1]
            boxes[hash(label)].pop(label, None)

    return sum((box_index + 1) * sum((lens_index + 1) * box[lens_label]
                for lens_index, lens_label in enumerate(box.keys()))
                for box_index, box in enumerate(boxes) if box)

hash = lambda s: reduce(lambda acc, c: (acc + ord(c)) * 17 % 256, s, 0)