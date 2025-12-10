from math import prod

def solve(input_data):
    games = [[{(a := raw_assignment.split(' '))[1]: int(a[0]) for raw_assignment in raw_set.split(', ')}
                    for raw_set in line.split(': ')[1].split('; ')]
                for line in input_data.splitlines()]
    return (part_1(games), part_2(games))

def part_1(games):
    contents = {'red': 12, 'green': 13, 'blue': 14}

    return sum(i + 1 for i, game in enumerate(games)
               if all(v <= contents[k] for s in game for k, v in s.items()))

def part_2(games):
    return sum(prod(max(s.get(colour, 0) for s in game)
            for colour in ('red', 'green', 'blue'))
        for game in games)