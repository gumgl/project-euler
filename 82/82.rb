=begin
After solving #83, this was easy to do with the simple trick of removing the
left edges and adding zero-cost source and target vertices connected to each side.
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
    for xd,yd in [[-1,0],[+1,0],[0,+1]]
		if (x+xd).between?(0,$size-1) and
			(y+yd).between?(0,$size-1) # If the neighbor exists

		graph.add_edge([x,y], [x+xd,y+yd])
		edge_weights_map[[[x,y], [x+xd,y+yd]]] = matrix[x+xd][y+yd]
      end
    end

    if y == 0 # Source side
    	graph.add_edge("source", [x,y])
    	edge_weights_map[["source", [x,y]]] = 0
    end
    if y == $size-1 # Target side
    	graph.add_edge([x,y], "target")
    	edge_weights_map[[[x,y],"target"]] = 0
    end
  end
end

# Find the minimal path
visitor = RGL::DijkstraVisitor.new(graph)
dijkstra = RGL::DijkstraAlgorithm.new(graph, edge_weights_map, visitor)

shortest_path = dijkstra.shortest_path("source", "target")[1..-2]

# Map it back to matrix values
real_path = shortest_path.map{|v| matrix[v[0]][v[1]]}

puts real_path.to_s
puts real_path.sum