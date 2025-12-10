from Graphs import ALGraph

def solve(input_data):
    return part_1(input_data.splitlines()), None

def part_1(input_lines):
    graph = ALGraph()

    for line in input_lines: # create all nodes first
        graph.add_node(line[:3])

    for line in input_lines: # connect nodes
        graph.add_neighbours(line[:3], line[5:].split())

    (a, b, _) = graph.Karger_2_cut(3)
    
    return len(a) * len(b)

# Part 2 automatically unlocked after completing every other 2023 problem