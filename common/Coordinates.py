from math import sqrt
from enum import Enum

class Point:
    def __init__(self, x = 0, y = 0, z = 0):
        self.x = x
        self.y = y
        self.z = z

    @staticmethod
    def from_tuple(t):
        return Point(*t)
    
    @staticmethod
    def unit(dimensions):
        return Point.from_tuple((1,) * dimensions)

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
    
    def abs(self):
        return Point(abs(self.x), abs(self.y), abs(self.z))
    
    def manhattan_distance(self, other: Point):
        return (other - self).manhattan_length()
    
    def manhattan_length(self):
        return abs(self.x) + abs(self.y) + abs(self.z)
    
    def euclidean_distance(self, other: Point, fast_comparative = False):
        """Straight line distance between two points"""
        return (other - self).euclidean_length(skip_sqrt=fast_comparative)

    def euclidean_length(self, skip_sqrt = False):
        """Straight line distance to origin"""
        length = self.x ** 2 + self.y ** 2 + self.z ** 2
        return length if skip_sqrt else sqrt(length)
    
    def __getitem__(self, key):
        return getattr(self, key)
    
    def __setitem__(self, key, value):
        return setattr(self, key, value)

class Direction2D(Enum):
    UP = Point(0, -1)
    RIGHT = Point(1, 0)
    DOWN = Point(0, 1)
    LEFT = Point(-1, 0)

class RectangularCuboid:
    def __init__(self, start, end = None):
        """For integer (lattice) representation, end represents the boundary and is not included in the rectangle.
        e.g. a cube of x-length 2 has (start, end) = (1,3)"""
        if end is None:
            self.lower = Point()
            self.upper = start
        else:
            self.lower = Point(min(start.x, end.x), min(start.y, end.y), min(start.z, end.z))
            self.upper = Point(max(start.x, end.x), max(start.y, end.y), max(start.z, end.z))

    def __contains__(self, point : Point):
        return point is not None and \
            self.lower.x <= point.x < self.upper.x and \
            self.lower.y <= point.y < self.upper.y and \
            self.lower.z <= point.z < self.upper.z
    
    def __str__(self):
        return f"{self.lower}~{self.upper}"
    
    def __add__(self, delta: Point):
        return RectangularCuboid(self.lower + delta, self.upper + delta)
    
    def translate(self, delta: Point):
        self.lower += delta
        self.upper += delta

    def area(self, padding = Point(0,0)):
        """Returns x * y, 0 if any dimension length <= 0
        
        :param padding: Optional padding in each dimension, on direction only (e.g. padding.x=1 adds 1 to the X dimension length)
        """
        return (max(0, self.upper.x - self.lower.x) + padding.x) * (max(0, self.upper.y - self.lower.y) + padding.y)
    
    def volume(self):
        """Returns x * y * z, 0 if any dimension length <= 0"""
        return max(0, self.upper.x - self.lower.x) * max(0, self.upper.y - self.lower.y) * max(0, self.upper.z - self.lower.z)
    
    def overlap_2d(self, other):
        return RectangularCuboid(Point(max(self.lower.x, other.lower.x), max(self.lower.y, other.lower.y)),
                    Point(min(self.upper.x, other.upper.x), min(self.upper.y, other.upper.y)))
    
    def lattice_points_2d(self, padding_lower = Point(0,0), padding_upper = Point(0,0)):
        return (Point(x, y) for x in range(self.lower.x - padding_lower.x, self.upper.x + padding_upper.x)
                            for y in range(self.lower.y - padding_lower.y, self.upper.y + padding_upper.y))
    
    def intersects(self, other: RectangularCuboid, dimensions: int, allowable_overlap = 0):
        return not ((self.upper.x < other.lower.x + allowable_overlap
                 or other.upper.x < self.lower.x  + allowable_overlap) and dimensions >= 1
                 or (self.upper.y < other.lower.y + allowable_overlap
                 or other.upper.y < self.lower.y  + allowable_overlap) and dimensions >= 2
                 or (self.upper.z < other.lower.z + allowable_overlap
                 or other.upper.z < self.lower.z  + allowable_overlap) and dimensions >= 3)

class Rectangle(RectangularCuboid):
    def __contains__(self, point : Point):
        return point is not None and \
            self.lower.x <= point.x < self.upper.x and \
            self.lower.y <= point.y < self.upper.y

class Polygon2D:
    def __init__(self, points):
        """From a list of CW or CCW points that define the polygon (order matters)."""
        self.points = points
    
    def area(self):
        """Computes area inside polygon using the shoelace formula"""
        return abs(sum(p1.x * (p2 := self.points[(i+1) % len(self.points)]).y - p2.x * p1.y for i, p1 in enumerate(self.points)) / 2)
    
    def perimeter(self):
        """Returns the sum of all edge lengths"""
        return sum(p1.euclidean_distance(self.points[(i+1) % len(self.points)]) for i, p1 in enumerate(self.points))