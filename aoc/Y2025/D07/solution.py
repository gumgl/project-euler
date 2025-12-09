def solve(input_data):
    input_lines = input_data.splitlines()
    width = len(input_lines[0])

    beam_counts = [0] * width
    beam_counts[input_lines[0].find('S')] = 1
    splits = 0
    for line in input_lines[1:]:
        prev_counts = beam_counts[:]
        for i in range(width):
            if line[i] == '^':
                if (prev_counts[i] > 0):
                    splits += 1
                if i > 0:
                    beam_counts[i - 1] += prev_counts[i]
                if i < width - 1:
                    beam_counts[i + 1] += prev_counts[i]
                beam_counts[i] = 0
    return splits, sum(beam_counts)

# working part 1 merged into part 2
def part_1(input_lines):
    beam_indices = {input_lines[0].find('S')}
    width = len(input_lines[0])
    splits = 0
    for line in input_lines[1:]:
        for beam in list(beam_indices):
            if line[beam] == '^':
                splits += 1
                beam_indices.remove(beam)
                if beam > 0:
                    beam_indices.add(beam - 1)
                if beam < width - 1:
                    beam_indices.add(beam + 1)
    return splits