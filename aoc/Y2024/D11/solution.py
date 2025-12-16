from functools import cache

def solve(input_data):
    stones = [int(line) for line in input_data.strip().split(' ')]

    return count_stones(stones, 25), count_stones(stones, 75)

def count_stones(stones, blinks):
    return sum(count_stone(stone, blinks) for stone in stones)

@cache
def count_stone(stone, blinks):
    if blinks == 0:
        return 1
    else:
        if stone == 0:
            return count_stone(1, blinks - 1)
        elif (l := len(s := str(stone))) % 2 == 0:
            return count_stone(int(s[:l//2]), blinks - 1) + count_stone(int(s[l//2:]), blinks - 1)
        else:
            return count_stone(stone * 2024, blinks - 1)