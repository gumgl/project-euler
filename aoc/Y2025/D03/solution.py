input_lines = open('03_input.txt', 'r').read().splitlines()

def largest_subnumber(n: str, length: int):
    """
    Returns the largest subnumber of given length from n

    Strategy: greedy selection of the highest most significant digit possible, then recurse
    
    :param n: number string to search
    :type n: str
    :param length: length of the substring to consider
    """
    highest = max(n[:len(n)-length+1])
    i = n.index(highest)
    return highest + largest_subnumber(n[i+1:], length-1) if length > 1 else highest

def part_1():
    return sum(int(largest_subnumber(n, 2)) for n in input_lines)

def part_2():
    return sum(int(largest_subnumber(n, 12)) for n in input_lines)

print(part_1())
print(part_2())