import re
from collections import deque
from math import prod, lcm

def solve(input_data):
    input_lines = input_data.splitlines()
    input_modules = {match.group(2): (match.group(1), match.group(3).split(', '))
                    for match in map(re.compile('([%&])?([a-z]+) -> ((?:[a-z]+, )*(?:[a-z]+))').fullmatch, input_lines) if match}  
    module_io = {module_id: input_modules[module_id] +
                    ([source_id for source_id, source_module in input_modules.items() if module_id in source_module[1]],)
                    for module_id in input_modules}
    
    return part_1(module_io), part_2(module_io)

def part_1(module_io):    
    return prod(simulate(module_io, target_button_presses=1000).values())

def part_2(module_io):
    # Assumption: one conjunction module leads to rx, and all modules leading to it will send a 
    # high pulse cyclically. Therefore we register the number of presses for each and compute the LCM.

    last_before_rx = next(i for i, m in module_io.items() if 'rx' in m[1])
    hopefully_cyclic = module_io[last_before_rx][2]

    return lcm(*simulate(module_io, targets=hopefully_cyclic))

def simulate(modules, target_button_presses = 0, targets = None, print_sequence = False):
    module_data = {}
    pulse_counts = {False: 0, True: 0}
    button_presses = 0
    if targets:
        target_counts = dict.fromkeys(targets)
    while button_presses < target_button_presses or targets:
        pulses = deque([(False, 'broadcaster', None)])
        button_presses += 1
        if print_sequence:
            input()
            print('button -low-> broadcaster')
        while pulses:
            (pulse_type, module_id, origin_id) = pulses.popleft()
            pulse_counts[pulse_type] += 1
            if targets and origin_id in targets and pulse_type:
                target_counts[origin_id] = button_presses
            if targets and all(vs := target_counts.values()):
                return vs
            if module_id in modules:
                (module_type, destinations, sources) = modules[module_id]
                next_pulse_type = None
                match module_type:
                    case '%': # flip-flop
                        state = module_data.get(module_id, False)
                        if not pulse_type: # low pulse
                            module_data[module_id] = not state
                            next_pulse_type = not state
                    case '&': # conjunction
                        state = module_data.get(module_id, {})
                        state[origin_id] = pulse_type
                        module_data[module_id] = state
                        next_pulse_type = not all(state.get(source_id, False) for source_id in sources)
                    case None: # broadcaster
                        next_pulse_type = pulse_type
                if next_pulse_type is not None:
                    if print_sequence:
                        for destination_id in destinations:
                            print(module_id, '-high->' if next_pulse_type else '-low->', destination_id)
                    pulses.extend((next_pulse_type, destination_id, module_id) for destination_id in destinations)
    return pulse_counts

def print_graph(input_modules):
    """GraphViz markup for use on https://edotor.net/"""
    print('digraph {')
    for module_id, module in input_modules.items():
        print(module_id, '-> {', *module[1], '};')
        match module[0]:
            case '%':
                print(module_id, '[shape=invtriangle, color=red];')
            case '&':
                print(module_id, '[shape=trapezium, color=green];')
            case None:
                print(module_id, '[shape=star, color=yellow];')
    print('}')