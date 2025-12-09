import itertools
import math

boxes = [tuple(map(int, line.split(','))) for line in open('08_input.txt', 'r').read().splitlines()]

def distance(b1, b2):
    return sum((c1 - c2) ** 2 for c1, c2 in zip(b1, b2))

circuits = []
box_to_circuit: dict[tuple[int, ...], set[tuple[int, ...]]] = {}
parts_completed = [False] * 2

sorted_pairs = sorted(itertools.combinations(boxes, 2), key=lambda p: distance(p[0], p[1]))
answers = {}

for i, (box1, box2) in enumerate(sorted_pairs):
    if answers.get(0) is None and i >= 1000:
        answers[0] = math.prod(sorted((len(c) for c in circuits), reverse=True)[:3])

    c1, c2 = box_to_circuit.get(box1), box_to_circuit.get(box2)

    if c1 and c2:
        if c1 is not c2:
            c1.update(c2)
            for b in c2:
                box_to_circuit[b] = c1
            c2.clear()
    elif c1:
        c1.add(box2)
        box_to_circuit[box2] = c1
    elif c2:
        c2.add(box1)
        box_to_circuit[box1] = c2
    else:
        new_c = {box1, box2}
        circuits.append(new_c)
        box_to_circuit[box1] = new_c
        box_to_circuit[box2] = new_c

    if answers.get(1) is None and len(box_to_circuit) == len(boxes):
        answers[1] = box1[0] * box2[0]

    if answers.get(0) and answers.get(1):
        break

print(answers.values())