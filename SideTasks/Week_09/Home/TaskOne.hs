import Data.List

-- Define a function that accepts a graph, a whole number k and a node n.
-- Return all the paths starting from n that are k nodes long. 
-- If the node is not present, throw an error.

-- Use the following types:

-- type Node = Int
-- type Graph = [(Node, [Node])]
-- type Path = [Node]

main :: IO()
main = do
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 == [[1]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 == [[1, 2, 3], [1, 2, 4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 == [[2,3],[2,4]]

type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

getNodePaths :: Graph -> Node -> [Path]
getNodePaths g n = [ n : [n1] | n1 <- ps]
    where ps = snd $ head $ filter (\ (n1, _) -> n1 == n) g

makePathList :: Graph -> Node -> [Path]
makePathList g n = ps ++ concat [ getNodePaths g (last p) | p <- ps]
    where ps = getNodePaths g n

connectPaths :: [Path] -> Node -> [Path]
connectPaths ps n = [ p1 ++ tail p2 | p1 <- ps, p2 <- tail ps, last p1 == head p2, p1 /= p2 ]

doesNodeExist :: Graph -> Node -> Bool
doesNodeExist [] n          = False
doesNodeExist ((n1, _):g) n = n == n1 || doesNodeExist g n

simplePaths :: Graph -> Int -> Node -> [Path]
simplePaths g k n
    | not (doesNodeExist g n) = error "No such node exists"
    | k == 0                  = [[n]]
    | otherwise               = acc 1 (getNodePaths g n)
        where
            acc :: Int -> [Path] -> [Path]
            acc x ps
                | x == k    = filter (\p -> length p == k + 1) $ nub ps
                | otherwise = acc (x + 1) (connectPaths (ps ++ makePathList g n) n)