input_sections = [section.splitlines() for section in open('05_input.txt', 'r').read().split('\n\n')]

input_seeds = [int(seed) for seed in input_sections[0][0].split(':')[1].split()]

maps = {(names := lines[0].split()[0].split('-'))[0] : 
         (names[2], sorted([[int(n) for n in line.split()] for line in lines[1:]], key=lambda line: line[1]))
         for lines in input_sections[1:]}

def map_seed(info_type, n):
    return next((n - source + destination
            for destination, source, length in maps[info_type][1]
            if n in range(source, source + length)),
            n)

def map_seed_range(info_type, seed_start, seed_length):
    seed_ranges = []
    n = seed_start
    rules = iter(maps[info_type][1])

    while n < seed_start + seed_length:
        if (rule := next(rules, None)) is None: # no more rule, leftover range
            seed_ranges.append((n, seed_start + seed_length - n))
            n = seed_start + seed_length
        else:
            map_destination, map_source, map_length = rule
            if n < map_source: # we are before a map rule
                start, end = n, min(map_source, seed_start + seed_length)
                seed_ranges.append((start, end - start)) # add range as-is
                n = end
            elif n < map_source + map_length: # we are within a map rule
                start = max(n, map_source) # in case we start in the middle of a rule
                end = min(map_source + map_length, seed_start + seed_length)
                seed_ranges.append((start + map_destination - map_source, end - start))
                n = end
            # If we are after current rule, do nothing until we find a rule in front of us
    return seed_ranges

def part_1():
    info_type = 'seed'
    seeds = input_seeds
    while info_type != 'location':
        seeds = [map_seed(info_type, seed) for seed in seeds]
        info_type = maps[info_type][0]

    return min(seeds)

def part_2():
    """Since there are almost 2B seeds, mapping them individually is too slow.
    Instead, we map seed ranges against all the rules of each map, partitioning the number of
    seed ranges by a small factor each time (proportional to the probability of partial overlap).
    """
    seed_ranges = [(input_seeds[i*2], input_seeds[i*2+1]) for i in range(len(input_seeds)//2)]
    info_type = 'seed'
    while info_type != 'location':
        seed_ranges = sum((map_seed_range(info_type, *seed_range) for seed_range in seed_ranges), [])
        info_type = maps[info_type][0]
        # print(len(seed_ranges)) # barely reaches above 100
    return min(seed_range[0] for seed_range in seed_ranges)

print(part_1())
print(part_2())