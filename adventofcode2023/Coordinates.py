class Point:
    def __init__(self, x = 0, y = 0, z = 0):
        self.x = x
        self.y = y
        self.z = z

    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y, self.z + other.z)
    
    def __sub__(self, other):
        return Point(self.x - other.x, self.y - other.y, self.z - other.z)
    
    def __mul__(self, scalar):
        """Scalar multiplication"""
        return Point(self.x * scalar, self.y * scalar, self.z * scalar)
    
    __rmul__ = __mul__
    
    def __eq__(self, other):
        return other is not None and self.x == other.x and self.y == other.y and self.z == other.z
        
    def __ne__(self, other):
        return not self.__eq__(other)
    
    def __lt__(self, other):
        return tuple(self) < tuple(other)
    
    def __iter__(self):
        yield self.x
        yield self.y
        yield self.z
        
    def __str__(self):
        return ("Point(%d, %d)" % (self.x, self.y) if self.z == 0
           else "Point(%d, %d, %d)" % (self.x, self.y, self.z))
    
    def __hash__(self):
        return hash((self.x, self.y, self.z))

    def __repr__(self):
        return self.__str__()
    
    def reverse(self):
        return self.__mul__(-1)
    
    def flip_2d(self):
        """Swap x and y values"""
        return Point(self.y, self.x, self.z)
    
    def euclidean_distance(self, other):
        return abs(other.x - self.x) + abs(other.y - self.y) + abs(other.z - self.z)

class Cube:
    def __init__(self, start, end):
        self.lower = Point(min(start.x, end.x), min(start.y, end.y), min(start.z, end.z))
        self.upper = Point(max(start.x, end.x), max(start.y, end.y), max(start.z, end.z))

    def __contains__(self, point):
        return (self.lower.x <= point.x <= self.upper.x
            and self.lower.y <= point.y <= self.upper.y
            and self.lower.z <= point.z <= self.upper.z)
    
    def area(self):
        return (self.upper.x - self.lower.x) * (self.upper.y - self.lower.y)
    
    def volume(self):
        return (self.upper.x - self.lower.x) * (self.upper.y - self.lower.y) * (self.upper.z - self.lower.z)
