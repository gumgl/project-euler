from collections import deque

def solve(input_data):
    cards = [tuple(set(map(int, number_set.split())) for number_set in line.split(': ')[1].split('|'))
            for line in input_data.splitlines()]
    matches = [len(winning & hand) for (winning, hand) in cards]

    return part_1(matches), part_2(matches)

def part_1(matches):
    return sum(2**(match - 1) if match > 0 else 0 for match in matches)

def part_2(matches):
    count = 0
    to_visit = deque(list(range(len(matches))))
    while to_visit:
        count += 1
        i = to_visit.popleft()
        to_visit.extend(range(i + 1, i + 1 + matches[i]))

    return count