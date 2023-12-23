input_file = open('15_input.txt', 'r')

input_commands = list(input_file.read().strip().split(','))

def hash(s):
  chars = list(*s.split())
  current_value = 0
  for c in chars:
    current_value += ord(c)
    current_value *= 17
    current_value %= 256

  return current_value

def part_1():
  return sum(hash(command) for command in input_commands)

def part_2():
  boxes = [{} for _ in range(256)]

  for command in input_commands:
    if command[-2] == '=':
      label = command[:-2]
      boxes[hash(label)][label] = int(command[-1])
    else:
      label = command[:-1]
      boxes[hash(label)].pop(label, None)
  
  return sum((box_index + 1) * sum((lens_index + 1) * box[lens_label]
                  for lens_index, lens_label in enumerate(box.keys()))
                  for box_index, box in enumerate(boxes) if box)

print(part_1())
print(part_2())