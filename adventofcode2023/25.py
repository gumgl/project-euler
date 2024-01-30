from Graphs import ALGraph

input_lines = open('25_input.txt', 'r').read().splitlines()

graph = ALGraph()

for line in input_lines: # create all nodes first
  graph.add_node(line[:3])

for line in input_lines: # connect nodes
  graph.add_neighbours(line[:3], line[5:].split())

def part_1():
  (a, b, _) = graph.Karger_2_cut(3)
  
  return len(a) * len(b)

print(part_1())
# Part 2 to be unlocked after completing every other 2023 problem