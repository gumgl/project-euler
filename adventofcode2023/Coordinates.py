class Point:
    def __init__(self, x = 0, y = 0):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y)
    
    def __sub__(self, other):
        return Point(self.x - other.x, self.y - other.y)
    
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y
        
    def __ne__(self, other):
        return not self.__eq__(other)
    
    def reverse(self):
        return Point(-1 * self.x, -1 * self.y)
        
    def __str__(self):
        return "(%d, %d)" % (self.x, self.y)
    
    def __repr__(self):
        return self.__str__()