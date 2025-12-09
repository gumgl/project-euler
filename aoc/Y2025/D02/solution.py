def solve(input_data):
    ranges = [[int(i) for i in idrange.split('-')] for idrange in input_data.split(',')]
    return (part_1(ranges), part_2(ranges))

divisors = lambda n : {j for i in range(1, int(n**0.5) + 1) if n % i == 0 for j in (i, n//i)}

# original part 1 solution, replaced by is_repeated_pattern
def is_repeated_twice(n):
    s, l = str(n), len(str(n))
    return l % 2 == 0 and s[:l//2] == s[l//2:]

def is_repeated_pattern(n, r):
    """
    Returns whether n consists of r repetitions of the same pattern
    
    :param n: number to check
    :param r: number of repetitions
    """
    s, l = str(n), len(str(n))

    return l % r == 0 and all(s[0:l//r] == s[i:i+(l//r)] for i in range(l//r, l, l//r))

def is_repeated_any(n):
    """
    Returns whether n consists of any repeated pattern
    
    :param n: number to check
    """
    return any(is_repeated_pattern(n, r) for r in (divisors(len(str(n))) - {1}))

def part_1(ranges):
    return sum(sum(ns for ns in list(range(idrange[0],idrange[1]+1)) if is_repeated_pattern(ns, 2)) for idrange in ranges)

def part_2(ranges):
    return sum(sum(ns for ns in list(range(idrange[0],idrange[1]+1)) if is_repeated_any(ns)) for idrange in ranges)