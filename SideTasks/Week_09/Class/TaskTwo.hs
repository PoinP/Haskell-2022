-- Define a function that checks whether a given path is 
-- valid in a graph g = (1, [2, 3]), (2, [3, 4]), (3, []), (4, []).

main :: IO()
main = do
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 2, 4] == True
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 3, 4] == False
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [2, 3] == True
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [3, 1] == False

type Node = Int
type Graph = [(Node, [Node])]

hasPath :: Graph -> Node -> Node -> Bool
hasPath g n1 n2 = elem n2 $ snd $ head $ filter (\ (x, y) -> x == n1) g

isPath :: Graph -> [Node] -> Bool
isPath g []  = False
isPath g [n] = True
isPath g (n1:n2:ns)
    | hasPath g n1 n2 = isPath g (n2:ns)
    | otherwise       = False