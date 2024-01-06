import copy

def indices_2d(haystack, value_or_predicate, nested_index_first = False):
    """Find indices in a 2-dimensional list, returned as iterable

    ### Parameters
        haystack: [[]]
            2D list to search
        predicate_or_value: Any
            if callable, will be used as predicate, otherwise equality is checked
        nested_index_first: Bool (default: False)
            flip returned coordinate format to (i2, i1)
    ### Returns
        iterator of tuples of the form (i1, i2) for each matching haystack[i1][i2]
    """
    return ((i2,i1) if nested_index_first else (i1,i2) for (i1,sublist) in enumerate(haystack) for (i2,e) in enumerate(sublist)
            if (value_or_predicate(e) if callable(value_or_predicate) else e == value_or_predicate))

def keys_by_value(haystack, value_or_predicate):
    """Reverse dictionary lookup: find keys for given value or predicate

    ### Parameters
        haystack: dict()
            dictionary to search
        predicate_or_value: Any
            if callable, will be used as predicate, otherwise equality is checked
    ### Returns
        iterator of keys such that haystack[key] == value or predicate(haystack[key]) == True
    """
    return (key for key, value in haystack.items() 
            if (value_or_predicate(value) if callable(value_or_predicate) else value == value_or_predicate))

def pairs(elems):
    """Return size 2 combinations (pairs) from a list

    ### Parameters
        elems: iterator or list
    ### Returns
        iterator of pair tuples e.g. [(elems[0],elems[1]),(elems[0],elems[2]),...]
    """
    elems = list(elems) # dynamic typing...
    return ((e1, e2) for i, e1 in enumerate(elems) for e2 in elems[i + 1:])

def count_if(haystack, value_or_predicate):
    """ Count the number of elements in iterator which match value or predicate"""
    return sum(value_or_predicate(elem) if callable(value_or_predicate) else elem == value_or_predicate for elem in haystack)

def count_while(haystack, value_or_predicate):
    """ Count the number of elements at the start of iterator which match value or predicate"""
    return next(index for index, elem in enumerate(haystack) if not
            (value_or_predicate(elem) if callable(value_or_predicate) else elem == value_or_predicate))

def count_first_elements(haystack):
    """Return the first element along with how many times it appears at the start of the list"""
    return (first_elem := next(haystack), 1 + count_while(haystack, first_elem))

def init_2d(rows, cols, value = 0):
    """Return a 2D list with specified dimensions and value.
    list[row][col] = value for row=[0..rows], col=[0..cols]"""
    return [[copy.deepcopy(value) for _ in range(cols)] for _ in range(rows)]