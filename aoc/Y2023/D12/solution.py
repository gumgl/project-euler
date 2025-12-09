from functools import lru_cache
from adventofcode2023.helpers import count_if

input_file = open('12_input.txt', 'r')

input_data = tuple(((*pattern, ), tuple(int(n) for n in group.split(',')))
              for (pattern, group) in (line.strip().split()
                                       for line in input_file.readlines()))

@lru_cache
def count_matches(pattern, groups):
  # State: if groups does not start with 0, we are currently in a group and need to finish (cannot accept '.')

  if not pattern: # only count 1 when successfully reaching end of conditions
    return groups == (0,) # valid if no leftover groups
    
  if not count_if(pattern, '#') <= sum(groups) <= count_if(pattern, lambda c: c in ('#', '?')):
    return 0 # invalid: not enough groups to fill mandatory #s or not enough #s or ?s to fill groups

  match pattern[0]:
    case '#': # damaged well      
      if groups[0] == 0: 
        groups = groups[1:] # drop leading zero

      if groups[0] == 1: # if at end of group
        if len(pattern) <= 1 or pattern[1] != '#': # make sure we are not forced to continue a complete group, or at end of input
          return count_matches(pattern[2:], (0,) + groups[1:]) # skip next char, leave 0 at start of groups
        else: # if we are forced to continue a complete group
          return 0 # invalid
      else:
        return count_matches(pattern[1:], (groups[0] - 1,) + groups[1:]) # move forward in group
    case '.': # operational well
      if groups[0] == 0: # if not in a group
        return count_matches(pattern[1:], groups) # move forward
      else: # if forced to end incomplete group
        return 0 # invalid
    case '?': # branch time!
      # assign ? = '.' or '#' and let the algorithm handle it in curr position
      return sum(count_matches((c,) + pattern[1:], groups) for c in ('.', '#'))
  return 0

def unfold(pattern, groups, factor):
  return (((pattern + ('?',)) * factor)[:-1], groups * factor)

def part(num):
  factors = [1,5]
  return sum(count_matches(pattern, (0,) + groups)
             for (pattern, groups) in [unfold(*data, factors[num - 1])
             for data in input_data])

print(part(1))
print(part(2))