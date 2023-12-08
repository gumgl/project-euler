from itertools import cycle
from math import lcm

inputfile = open('08_input.txt', 'r')

directions = [*inputfile.readline().strip()] # using * to unpack string

links_dict = {line[0:3]:(line[7:10],line[12:15]) for line in inputfile.readlines()[1:]}

def traverse(node, end_condition, require_cycle):
  direction = cycle(directions)
  step = 0
  start_links = links_dict[node]

  # Note: putting cycle requirements inside the search allows the search to continue if a --Z node appears out of cycle
  while not (end_condition(node) and (not require_cycle or (step >= 1 and links_dict[node] == start_links and step % len(directions) == 0))):
    node = links_dict[node][0 if next(direction) == 'L' else 1]
    step += 1
  
  return step, node

def part_1():
  return traverse('AAA', lambda key: key == 'ZZZ', False)[0]

# For part 2, a few properties of the input graph are necessary in order to use LCM:
# 1- The path must eventually end in a cycle (i.e. --Z leads back to --Z).
# 2- The first traverse from --A to --Z must be the same length at the --Z cycle.
#    Note: It just so happens to be the case despite those paths being entirely different!
# 3- To have a --Z cycle, the same directions are also needed. Thus, cycle length must be a multiple of len(directions).

def part_2():
  start_nodes = [key for (key,_) in links_dict.items() if key[2] == 'A']
  
  # First traverse from --A to --Z
  traverse_lengths, traverse_end_nodes = map(list,zip(*[traverse(node, lambda key: key[2] == 'Z', False) for node in start_nodes]))
  
  # Second traverse searching for cycle from --Z back to --Z
  cycle_lengths, cycle_end_nodes = map(list,zip(*[traverse(node, lambda key: key[2] == 'Z', True) for node in traverse_end_nodes]))

  return lcm(*cycle_lengths) if traverse_end_nodes == cycle_end_nodes and traverse_lengths == cycle_lengths else -1

print(part_1())
print(part_2())

# def node2str(node):
#   return "%s = (%s,%s)" % (node, links_dict[node][0], links_dict[node][1])

# Parsing for variable-length IDs:
# import re
# links_raw = inputfile.read()
# links_tuples = re.findall(r'([A-Z]+) = \(([A-Z]+), ([A-Z]+)\)\n', links_raw)
# links_dict = h = {k:(l,r) for k,l,r in links_tuples}