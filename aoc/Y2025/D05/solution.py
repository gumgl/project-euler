def solve(input_data):
    input_sections = input_data.split('\n\n')
    ranges = sorted([[int(n) for n in line.split('-')] for line in input_sections[0].splitlines()], key=lambda r: r[0])
    ids = [int(line) for line in input_sections[1].splitlines()]

    return (part_1(ranges, ids), part_2(ranges))

def part_1(ranges, ids):
    return sum(any(r[0] <= id <= r[1] for r in ranges) for id in ids)

def part_2(ranges):
    merged = []
    current_start, current_end = ranges[0]
    
    for i in range(1, len(ranges)):
        next_start, next_end = ranges[i]
        
        # merge the next range if overlapping or adjacent
        if next_start <= current_end + 1:
            current_end = max(current_end, next_end)
        else:
            merged.append((current_start, current_end))
            current_start, current_end = next_start, next_end
            
    merged.append((current_start, current_end))
    
    return sum(end - start + 1 for start, end in merged)