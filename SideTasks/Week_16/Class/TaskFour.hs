main :: IO()
main = do
    print $ deepestNodesSum odd t1 == 7
    print $ deepestNodesSum even t2 == 4

data BTree = Empty | Node Int BTree BTree

t1 :: BTree
t1 = Node 1 (Node 2 (Node 4 (Node 7 Empty Empty) Empty) (Node 5 Empty Empty)) (Node 3 Empty (Node 6 Empty (Node 8 Empty Empty)))

t2 :: BTree
t2 = Node 1 (Node 2 (Node 4 Empty Empty) Empty) (Node 3 Empty Empty)

deepestNodesSum :: (Int -> Bool) -> BTree -> Int
deepestNodesSum _ Empty = 0
deepestNodesSum p t = sum $ filter p (getLevel t (getTreeSize t - 1))

getLevel :: BTree -> Int -> [Int]
getLevel Empty _ = []
getLevel (Node v l r) 0 = [v]
getLevel (Node v l r) n = getLevel l (n - 1) ++ getLevel r (n - 1)

getTreeSize :: BTree -> Int
getTreeSize Empty = 0
getTreeSize (Node v l r) = 1 + max (getTreeSize l) (getTreeSize r)