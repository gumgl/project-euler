from itertools import cycle
from math import lcm

inputfile = open('08_input.txt', 'r')

directions = [*inputfile.readline().strip()] # using * to unpack string

links_dict = {line[0:3]:(line[7:10],line[12:15]) for line in inputfile.readlines()[1:]}

def traverse(node, end_condition):
  direction = cycle(directions)
  steps = 0

  while not end_condition(node):
    node = links_dict[node][0 if next(direction) == 'L' else 1]
    steps += 1

  return steps

def part_1():
  return traverse('AAA', lambda key: key == 'ZZZ')

def part_2():
  nodes = [key for (key,_) in links_dict.items() if key[2] == 'A']
  lengths = [traverse(node, lambda key: key[2] == 'Z') for node in nodes]
  
  return lcm(*lengths)

print(part_1())
print(part_2())

# Parsing for variable-length IDs:
# import re
# links_raw = inputfile.read()
# links_tuples = re.findall(r'([A-Z]+) = \(([A-Z]+), ([A-Z]+)\)\n', links_raw)
# links_dict = h = {k:(l,r) for k,l,r in links_tuples}