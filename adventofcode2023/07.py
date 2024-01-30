from enum import Enum
from adventofcode2023.helpers import count_if

input_lines = open('07_input.txt', 'r').read().splitlines()
HandType = Enum('HandType', ['HIGH_CARD', 'ONE_PAIR', 'TWO_PAIR', 'THREE_OF_A_KIND','FULL_HOUSE', 'FOUR_OF_A_KIND', 'FIVE_OF_A_KIND'])

class Hand:
    labels = ([*'23456789TJQKA'], [*'J23456789TQKA'])
    def __init__(self, cards, bid, joker = False):
        self.cards = cards
        self.bid = bid
        self.joker = joker
    
    def type(self):
        counts = {label: count_if(self.cards, label) for label in Hand.labels[self.joker]}

        if self.joker:
            J_counts = counts['J']
            counts['J'] = 0
            counts[max(counts, key=counts.get)] += J_counts
        
        if 5 in counts.values():
            return HandType.FIVE_OF_A_KIND
        elif 4 in counts.values():
            return HandType.FOUR_OF_A_KIND
        elif 3 in counts.values() and 2 in counts.values():
            return HandType.FULL_HOUSE
        elif 3 in counts.values():
            return HandType.THREE_OF_A_KIND
        elif count_if(counts.values(), 2) == 2:
            return HandType.TWO_PAIR
        elif 2 in counts.values():
            return HandType.ONE_PAIR
        else:
            return HandType.HIGH_CARD

    def __lt__(self, other):
        return (self.type().value < other.type().value if self.type() != other.type()
                else tuple(Hand.labels[self.joker].index(card) for card in self.cards)
                   < tuple(Hand.labels[self.joker].index(card) for card in other.cards))

def part(num):
    hands = [Hand([*line[:5]], int(line[6:]), num == 2) for line in input_lines]
    return sum((i + 1) * hand.bid for i, hand in enumerate(sorted(hands)))

print(part(1))
print(part(2))