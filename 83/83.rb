=begin Uses the same input file as in #81

Here I used RGL's implementation of Dijkstra's algorithm to find the minimal
path, so the main challenge was setting up the graph correctly. Specifically,
the algorithm minimizes edge weights whereas we only have vertex weights.
Instead of creating an edge graph, we can do something simpler:

Let G(V,E) be a new  directed weighted graph and M[x,y] be the matrix provided.
V = {v : v is a position in M}
    Note that we store the coordinate pair as v to easily compute the sum at the end.
E = {u,v : u and v are adjacent in M (i.e. Manhattan distance = 1)}
    Note this implies two edges in opposite directions between each adjacent positions.
w(u,v) = {M[x,y] where x,y are the coordinates of v}

In other words, the cost of travelling through a vertex v is factored into every edge
leading to v.
=end

require 'rgl/adjacency'
require 'rgl/dijkstra'

lines = File.readlines('matrix.txt')
matrix = lines.map {|string| string.split(',').map {|n| Integer(n, 10)}}

$size = matrix.length # Should be == 80
graph = RGL::DirectedAdjacencyGraph.new
edge_weights_map = Hash.new

# Build graph
for x in 0..($size-1)
  for y in 0..($size-1)
    for xd,yd in [[-1,0],[+1,0],[0,-1],[0,+1]]
      if (x+xd).between?(0,$size-1) and
          (y+yd).between?(0,$size-1) # If the neighbor exists

        graph.add_edge([x,y], [x+xd,y+yd])
        edge_weights_map[[[x,y], [x+xd,y+yd]]] = matrix[x+xd][y+yd]
      end
    end
  end
end

# Find the minimal path
visitor = RGL::DijkstraVisitor.new(graph)
dijkstra = RGL::DijkstraAlgorithm.new(graph, edge_weights_map, visitor)

shortest_path = dijkstra.shortest_path([0,0], [$size-1, $size-1])

# Map it back to matrix values
real_path = shortest_path.map{|v| matrix[v[0]][v[1]]}

puts real_path.to_s
puts real_path.sum