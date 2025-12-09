def solve(input_data):
    return (part_1(input_data.strip()), part_2(input_data.strip()))

def part_1(input_line):
    file_sizes = [int(input_line[n]) for n in range(0, len(input_line), 2)]
    space_sizes = [int(input_line[n]) for n in range(1, len(input_line), 2)]

    checksum = 0
    position = 0
    file_id = 0

    while file_id < len(file_sizes): # while we still have files to process

        checksum += sum(pos * file_id for pos in range(position, position + file_sizes[file_id]))
        position += file_sizes[file_id]

        while space_sizes[file_id] > 0 and file_id < len(file_sizes) - 1: # space to fill and farther file to move
            move_size = min(space_sizes[file_id], file_sizes[-1])

            file_sizes[-1] -= move_size
            space_sizes[file_id] -= move_size

            checksum += sum(pos * (len(file_sizes) - 1) for pos in range(position, position + move_size))
            position += move_size
            
            if file_sizes[-1] == 0:
                file_sizes.pop()  # remove empty file
        
        file_id += 1
    
    return checksum

def part_2(input_line):
    files = [[id, int(input_line[pos])] for id, pos in enumerate(range(0, len(input_line), 2))]
    spaces = [[int(input_line[n]), []] for n in range(1, len(input_line), 2)]

    for i, file_size in reversed(files): # files to move
        # Only search for space to the left of the current file
        if (destination := next((j for j in range(i) if file_size <= spaces[j][0]), None)) is not None:
            spaces[destination][0] -= file_size # reduce available space
            spaces[destination][1].append((i, file_size)) # record moved file
            files[i][0] = 0 # set ID to 0 to mark empty disk space
    
    checksum = 0
    position = 0
    
    for i, file in enumerate(files): # count unmoved files
        checksum += sum(pos * file[0] for pos in range(position, position + file[1]))
        position += file[1]

        if i <= len(spaces) - 1:
            for moved_file_id, moved_file_size in spaces[i][1]: # count moved files
                checksum += sum(pos * moved_file_id for pos in range(position, position + moved_file_size))
                position += moved_file_size
            position += spaces[i][0] # jump over empty space
    return checksum