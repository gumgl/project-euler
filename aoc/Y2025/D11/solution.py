from functools import cache

def solve(input_data):
    devices = {line[0:3]: line[5:].split(' ') for line in input_data.strip().splitlines()}

    @cache
    def visit(current, dac, fft):
        if current == 'out':
            return int(dac and fft)

        dac = dac or current == 'dac'
        fft = fft or current == 'fft'

        total_paths = 0
        for neighbor in devices.get(current, []):
            total_paths += visit(neighbor, dac, fft)
        
        return total_paths
    
    return visit('you', True, True), visit('svr', False, False)