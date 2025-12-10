from functools import partial

def solve(input_data):
    histories = [[int(n) for n in line.split()] for line in input_data.splitlines()]

    return part(histories, 1), part(histories, 2)

def part(histories, i):
    return sum(map(partial(next_value, i == 2), histories))

def derivative(seq):
    return [j-i for i, j in zip(seq[:-1], seq[1:])]

def next_value(add_to_beginning, sequence):
    seq_stack = [sequence.copy()]

    while not all(n == 0 for n in seq_stack[-1]):
        seq_stack.append(derivative(seq_stack[-1]))

    for i, seq in reversed(list(enumerate(seq_stack))):
        if i < len(seq_stack) - 1: # skip last line. not using [:-1] to refer to last line
            if add_to_beginning:
                seq.insert(0, seq[0] - seq_stack[i+1][0])
            else:
                seq.append(seq[-1] + seq_stack[i+1][-1])
    
    return seq_stack[0][0 if add_to_beginning else -1]