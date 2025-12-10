from Poker import Hand

def part(input_lines, num):
    hands = [Hand([*line[:5]], int(line[6:]), num == 2) for line in input_lines]
    return sum((i + 1) * hand.bid for i, hand in enumerate(sorted(hands)))

def solve(input_data):
    input_lines = input_data.splitlines()
    return part(input_lines, 1), part(input_lines, 2)