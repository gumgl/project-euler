import random
import copy

# Undirected, weighted graph built with adjacency lists
class ALGraph:
    def __init__(self):
        self.node_values = {}
        self.edge_weights = {}

    def node_ids(self):
        return self.edge_weights.keys()

    def all_edges(self):
        """Gives (node, neighbour, weight) for each edge"""
        return ((node, neighbour, weight) for node, weights in self.edge_weights.items() for neighbour, weight in weights.items())

    def size(self):
        return len(self.node_values)

    def add_node(self, id, neighbours = set(), value = None):
        """Add node with neighbours as an optional interable or dictionary of weights"""
        self.node_values[id] = value # overwrite, if this node was created from others' neighbours there would be no previous value

        if id not in self.edge_weights:
            self.edge_weights[id] = {}

        if neighbours:
            self.add_neighbours(id, neighbours)

    def remove_node(self, id):
        for neighbour_id in self.edge_weights[id]:
            if neighbour_id != id:
                del self.edge_weights[neighbour_id][id]
        del self.edge_weights[id]
        del self.node_values[id]

    def add_neighbours(self, id, neighbours):
        if type(neighbours) is dict:
            self.edge_weights[id].update(neighbours)
            for (neighbour_id, weight) in neighbours.items():
                if neighbour_id not in self.edge_weights: # create missing neighbour
                    self.add_node(neighbour_id)
            self.edge_weights[neighbour_id][id] = weight
        else:
            for neighbour_id in neighbours:
                if neighbour_id not in self.edge_weights: # create missing neighbour
                    self.add_node(neighbour_id)
            self.connect(id, neighbour_id)

    def connect(self, id1, id2, weight = 1):
        self.edge_weights[id1][id2] = weight
        self.edge_weights[id2][id1] = weight

    def connect_or_reinforce(self, id1, id2, weight = 1):
        """Add edge with weight=1 or increase edge by 1"""
        self.edge_weights[id1][id2] = self.edge_weights[id1].get(id2, 0) + weight
        self.edge_weights[id2][id1] = self.edge_weights[id2].get(id1, 0) + weight

    def disconnect(self, id1, id2):
        del self.edge_weights[id1][id2]
        del self.edge_weights[id2][id1]

    def merge_nodes(self, id1, id2):
        """Merge nodes id2 into id1"""
        for neighbour_id, weight in self.edge_weights[id2].items():
            if neighbour_id != id1:
                self.connect_or_reinforce(id1, neighbour_id, weight)

        self.remove_node(id2)

    # https://en.wikipedia.org/wiki/Karger%27s_algorithm
    def Karger_2_cut(self, target_cut_size):
        """Return the nodes on either side of a cut of target size or smaller using Karger's algorithm"""
        runtime = 0
        while runtime <= self.size(): # try at least a number of times
            runtime += 1
            graph = copy.deepcopy(self)
            graph.node_values = {id: {id} for id in graph.node_values.keys()} # store ids
            while graph.size() > 2:
                (id1, id2, _) = graph.get_random_edge_proportionally()
                graph.node_values[id1].update(graph.node_values[id2]) # copy node ids when merging
                graph.merge_nodes(id1, id2)
            
            # at this point there should remain only two nodes and one edge
            (id1, id2, weight) = next(graph.all_edges())
            if weight <= target_cut_size:
                return (graph.node_values[id1], graph.node_values[id2], weight)

        return None

    def get_random_node(self):
        return random.choice(list(self.node_ids()))

    def get_random_edge(self):
        return random.choice(list(self.all_edges()))

    def get_random_edge_proportionally(self):
        """Get a random edge proportionally to the weights"""
        return random.choices(tuple(self.all_edges()), weights=tuple(weight for (_, _, weight) in self.all_edges()), k=1)[0]
