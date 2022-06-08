main :: IO()
main = do
    print $ deepestLeavesSum t3 == 15
    print $ deepestLeavesSum t4 == 4

data BTree = Empty | Node Int BTree BTree

t3 :: BTree
t3 = Node 1 (Node 2 (Node 4 (Node 7 Empty Empty) Empty) (Node 5 Empty Empty)) (Node 3 Empty (Node 6 Empty (Node 8 Empty Empty)))

t4 :: BTree
t4 = Node 1 (Node 2 (Node 4 Empty Empty) Empty) (Node 3 Empty Empty)

deepestLeavesSum :: BTree -> Int
deepestLeavesSum t = sum $ getLevel t (getSize t - 1)

getSize :: BTree -> Int
getSize Empty = 0
getSize (Node v l r) = 1 + max (getSize l) (getSize r)

getLevel :: BTree -> Int -> [Int]
getLevel Empty _ = []
getLevel (Node v _ _) 0 = [v]
getLevel (Node v l r) i = getLevel l (i - 1) ++ getLevel r (i - 1)