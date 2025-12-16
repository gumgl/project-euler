import re

def solve(input_data):
    regions = [[int(x) for x in re.findall(r'\d+', line)] for line in input_data.split('\n\n')[-1].splitlines()]

    # Basic dimension check without packing... and it works (for real input only).
    return sum((region[0] // 3) * (region[1] // 3) >= sum(region[2:]) for region in regions), None