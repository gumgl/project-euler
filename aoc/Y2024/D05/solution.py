from functools import cmp_to_key

input_sections = open('05_input.txt', 'r').read().split('\n\n')

orders = [[int(n) for n in line.split('|')] for line in input_sections[0].splitlines()]
updates = [[int(n) for n in line.split(',')] for line in input_sections[1].splitlines()]

is_misordered = lambda update: any(all(n in update for n in order) and update.index(order[0]) > update.index(order[1]) for order in orders)

def part_1():
    return sum(update[len(update) // 2] for update in updates if not is_misordered(update))

def part_2():
    compare = lambda a, b: 1 if [a, b] in orders else -1 if [b, a] in orders else 0

    return sum(sorted(update, key=cmp_to_key(compare))[len(update) // 2] for update in updates if is_misordered(update))

print(part_1())
print(part_2())