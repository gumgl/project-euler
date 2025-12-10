import itertools
import math

from Coordinates import Point

def solve(input_data):
    boxes = [Point.from_tuple(int(n) for n in line.split(',')) for line in input_data.splitlines()]

    circuits = []
    box_to_circuit: dict[Point, set[Point]] = {}

    sorted_pairs = sorted(itertools.combinations(boxes, 2), key=lambda p: p[0].euclidean_distance(p[1], fast_comparative=True))
    answers = {}

    for i, (box1, box2) in enumerate(sorted_pairs):
        if answers.get(0) is None and i >= (1000 if len(boxes) > 20 else 10):
            # Weird case where one variable is different between real and example input yet not in the input files
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
            answers[1] = box1.x * box2.x

        if answers.get(0) and answers.get(1):
            break

    return answers.values()