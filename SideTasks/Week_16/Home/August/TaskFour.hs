import Data.List

main :: IO()
main = do
    print $ findNodes t1 == [12, 17, 20]
    print $ findNodes t2 == [11]

data BTree = Empty | Node Int BTree BTree

getSumNode :: BTree -> Int
getSumNode (Node v (Node v1 _ _) (Node v2 _ _)) = v1 + v2
getSumNode (Node v (Node v1 _ _) _) = v1
getSumNode (Node v _ (Node v2 _ _)) = v2
getSumNode _ = maxBound :: Int

-- Is maxBound a valid solution??

findNodes :: BTree -> [Int]
findNodes Empty = []
findNodes n@(Node v l r)
    | getSumNode n < v = sort $ v : findNodes l ++ findNodes r
    | otherwise        = sort $ findNodes l ++ findNodes r

t1 :: BTree
t1 = Node 1 (Node 12 (Node 2 Empty Empty) (Node 3 Empty Empty)) (Node 20 (Node 17 (Node 13 Empty Empty) Empty) Empty)

t2 :: BTree
t2 = Node 10 (Node 2 Empty (Node 3 (Node 4 Empty Empty) Empty)) (Node 11 (Node 1 Empty Empty) (Node 6 Empty Empty))