from math import prod

input_sections = [section.splitlines() for section in open('19_input.txt', 'r').read().split('\n\n')]

input_workflows = {name: rules.split(',') for name, rules in map(lambda line: line[:-1].split('{'), input_sections[0])}
input_parts = [{rating[0]: int(rating[2:]) for rating in ratings[1:-1].split(',')} for ratings in input_sections[1]]

def evaluate_part(part, workflow_set):
    workflow = 'in'
    while True:
        if workflow in ('A', 'R'):
            return workflow == 'A'
        rules = workflow_set[workflow]
        for i, rule in enumerate(rules):
            (condition, outcome) = (None, rule) if i == len(rules) - 1 else rule.split(':')
            if (condition is None or
                condition[1] == '<' and part[condition[0]] < int(condition[2:]) or
                condition[1] == '>' and part[condition[0]] > int(condition[2:])):
                workflow = outcome
                break

def part_1():
    return sum(sum(part.values()) for part in input_parts if evaluate_part(part, input_workflows))

def compute_ranges(workflow_set):
    """Returns the (min, max) range for each variable in each computation path of a workflow set"""
    to_visit = [('in', [])]
    accept_paths = []
    while to_visit:
        (workflow, conditions) = to_visit.pop()
        if workflow == 'A':
            accept_paths.append(conditions)
        elif workflow != 'R':
            rules = workflow_set[workflow]
            for i, rule in enumerate(rules):
                if i < len(rules) - 1:
                    (condition, outcome) = rule.split(':')
                    # Go down branch where rule is satisfied
                    to_visit.append((outcome, conditions + [(condition[0], condition[1], int(condition[2:]))]))
                    # Store opposite of condition for the rest of the rules
                    conditions.append((condition[0],
                                       '<' if condition[1] == '>' else '>',
                                       int(condition[2:]) + (1 if condition[1] == '>' else -1)))
                else: # default option, just go to that workflow
                    to_visit.append((rule, conditions))
    
    return [{rating:(max((val + 1 for (var, op, val) in condition if var == rating and op == '>'), default = 1) ,
                    min((val - 1 for (var, op, val) in condition if var == rating and op == '<'), default = 4000))
                for rating in 'xmas'}
            for condition in accept_paths]

def part_2():
    return sum(prod(maximum - minimum + 1 for minimum, maximum in ranges.values()) for ranges in compute_ranges(input_workflows))

print(part_1())
print(part_2())