from math import prod

games = [[{(a := raw_assignment.split(' '))[1]: int(a[0]) for raw_assignment in raw_set.split(', ')}
                for raw_set in line.split(': ')[1].split('; ')]
            for line in open('02_input.txt', 'r').read().splitlines()]

def part_1():
    contents = {'red': 12, 'green': 13, 'blue': 14}

    return sum(i + 1 for i, game in enumerate(games)
               if all(v <= contents[k] for s in game for k, v in s.items()))

def part_2():
    return sum(prod(max(s.get(colour, 0) for s in game)
            for colour in ('red', 'green', 'blue'))
        for game in games)

print(part_1())
print(part_2())