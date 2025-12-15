from functools import reduce
import Helpers
import z3

def solve(input_data):
    target_lights = [{i for i,c in enumerate(line[1:line.find(']')]) if c == '#'} for line in input_data.splitlines()]
    buttons = [[{int(i) for i in button[1:-1].split(',')}
                for button in line[line.find(']')+2:line.find('{')-1].split(' ')]
                for line in input_data.splitlines()]
    target_counters = [[int(i) for i in line[line.find('{')+1:-1].split(',')] for line in input_data.splitlines()]

    return part_1(buttons, target_lights), part_2(buttons, target_counters)

def part_1(buttons : list[list[set[int]]], target_lights: list[set[int]]):
    # Since there is no point in pressing a button more than once, we just try all combinations of buttons.
    # Max number of buttons is 10 so 2^10 combinations to check.
    return sum(min(len(combo) for combo in Helpers.all_combinations(buttons[i])
                   if reduce(set.symmetric_difference, combo, set()) == target_lights[i])
               for i in range(len(target_lights)))

def part_2(buttons : list[list[set[int]]], target_counters: list[list[int]]):
    result = 0

    for machine_i in range(len(target_counters)):
        solver = z3.Optimize()
        press_counts = [z3.Int(f'c_{j}') for j in range(len(buttons[machine_i]))]
        for c in press_counts:
            solver.add(c >= 0)
        solver.minimize(sum(press_counts))

        # Each target must equal sum of presses of buttons that affect it
        for target_i, target in enumerate(target_counters[machine_i]):
            solver.add(target == sum(c for j, c in enumerate(press_counts) if target_i in buttons[machine_i][j]))

        if solver.check() == z3.sat:
            result += sum(solver.model()[c].as_long() for c in press_counts)

    return result