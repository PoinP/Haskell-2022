data BTree = Empty | Node Int BTree BTree

-- deepestNodesSum :: (Int -> Bool) -> BTree -> Int
-- deepestNodesSum _ Empty = 0

-- I do not know what I need to do!

getLevel :: BTree -> Int -> [Int]
getLevel Empty _ = []
getLevel (Node v l r) 0 = [v]
getLevel (Node v l r) n = getLevel l (n - 1) ++ getLevel r (n - 1)

getTreeSize :: BTree -> Int
getTreeSize Empty = 0
getTreeSize (Node v l r) = 1 + max (getTreeSize l) (getTreeSize r)