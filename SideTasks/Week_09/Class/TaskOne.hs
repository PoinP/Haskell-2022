import Data.List

-- Given a directed graph g with edges [(1, 2), (1, 3), (2, 3), (2, 4)] 
-- define the following functions:

-- nodes - returns all the nodes of "g";
-- neighbours - accepts a node and returns its neighbours;
-- adjacencyList - returns the children of every parent.
-- Implementation detail: Create types for the graph structure.

main :: IO()
main = do
    print $ nodes [(1, 2), (1, 3), (2, 3), (2, 4)] == [1, 2, 3, 4]

    print $ neighbours 2 [(1, 2), (1, 3), (2, 3), (2, 4)] == [3, 4]
    print $ neighbours 4 [(1, 2), (1, 3), (2, 3), (2, 4)] == []

    print $ adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)] == [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]

type Node = Int
type Edge = (Node, Node)
type Graph = [Edge]

nodes :: Graph -> [Node]
nodes = sort . nub . foldr (\ (x, y) s -> x : y : s) []

neighbours :: Node -> Graph -> [Node]
neighbours n g = [ y | (x, y) <- g, x == n ]

adjacencyList :: Graph -> [(Node, [Node])]
adjacencyList g = [ (n, neighbours n g) | n <- nodes g ]
