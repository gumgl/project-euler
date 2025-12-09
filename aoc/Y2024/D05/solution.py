from functools import cmp_to_key

def solve(input_data):
    input_sections = input_data.split('\n\n')

    orders = [[int(n) for n in line.split('|')] for line in input_sections[0].splitlines()]
    updates = [[int(n) for n in line.split(',')] for line in input_sections[1].splitlines()]

    def is_misordered(update):
        return any(all(n in update for n in order) and update.index(order[0]) > update.index(order[1]) for order in orders)

    def part_1():
        return sum(update[len(update) // 2] for update in updates if not is_misordered(update))

    def part_2():
        compare = lambda a, b: 1 if [a, b] in orders else -1 if [b, a] in orders else 0

        return sum(sorted(update, key=cmp_to_key(compare))[len(update) // 2] for update in updates if is_misordered(update))
    
    return part_1(), part_2()