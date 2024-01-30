from collections import deque

cards = [tuple(set(map(int, number_set.split())) for number_set in line.split(': ')[1].split('|'))
        for line in open('04_input.txt', 'r').read().splitlines()]
matches = [len(winning & hand) for (winning, hand) in cards]


def part_1():
    return sum(2**(match - 1) if match > 0 else 0 for match in matches)

def part_2():
    count = 0
    to_visit = deque(list(range(len(matches))))
    while to_visit:
        count += 1
        i = to_visit.popleft()
        to_visit.extend(range(i + 1, i + 1 + matches[i]))

    return count

print(part_1())
print(part_2())