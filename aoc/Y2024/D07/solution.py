import operator
import itertools

input_lines = open('07_input.txt', 'r').read().splitlines()
equations = {int(line[:line.find(':')]): [int(n) for n in line.split(' ')[1:]] for line in input_lines}
concat = lambda a, b: int(str(a) + str(b))
real_operators = {'+': operator.add, '*': operator.mul, '|': concat}


def left_eval(operands, operators):
    result = operands[0]
    for i in range(len(operators)):
        result = real_operators[operators[i]](result, operands[i+1])
    return result

def solve(allowed_operators):
    return sum(value for value, operands in equations.items()
               if any(left_eval(operands, operators) == value
                      for operators in itertools.product(allowed_operators, repeat=len(operands)-1)))

print(solve(list(real_operators.keys())[:-1]))
print(solve(real_operators.keys()))