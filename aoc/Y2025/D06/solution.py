import operator
from functools import reduce

def solve(input_data):
    input_lines = input_data.splitlines()
    operand_lines = input_lines[:-1]
    operator_line = input_lines[-1]
    real_operators = {'+': operator.add, '*': operator.mul}
    real_initials = {'+': 0, '*': 1}

    def part_1():
        transpose = lambda matrix: [list(row) for row in zip(*matrix)]
        operands = transpose([[int(n) for n in line.split()] for line in operand_lines])
        operators = operator_line.split()
        return sum(reduce(real_operators[operators[i]], operands[i], real_initials[operators[i]]) for i in range(len(operators)))

    def part_2():
        # Indexes of the operations in the operator line
        operation_is = [i for i, e in enumerate(operator_line) if e == '+' or e == '*']
        
        return sum((reduce(real_operators[operator_char := operator_line[operator_i]],
                    [int(''.join(operand_lines[digit_i][operand_i] for digit_i in range(len(operand_lines)))) # digits combined
                        for operand_i in range(operator_i, operation_is[operation_i+1]-1 # for each operand
                                            if operation_i + 1 < len(operation_is) else len(operator_line))],
                    real_initials[operator_char])
                    for operation_i, operator_i in enumerate(operation_is)))
    
    return part_1(), part_2()