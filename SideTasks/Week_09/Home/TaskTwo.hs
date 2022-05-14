import Data.List

-- Let (x, y, z) be a vector representing the nodes of a 
-- binary tree such that x is the value of the current node, 
-- y and z are the values of the child nodes. Define a function 
-- that returns the leaves of such a tree.

main :: IO()
main = do
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] == [3, 4, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]

type Node = Int
type Branch = (Node, Node, Node)

getChildren :: Branch -> [Node]
getChildren (n1, n2, n3) = [n2, n3]

searchChildren :: [Branch] -> Node -> [Node]
searchChildren bs n
    | null b    = []
    | otherwise = getChildren $ head b
        where b = filter (\(node, _, _) -> node == n) bs

listLeaves :: [Branch] -> [Node]
listLeaves []     = []
listLeaves bs = [ c | b <- sort bs, c <- getChildren b, null $ searchChildren bs c]