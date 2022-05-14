main :: IO()
main = do
    print $ grandchildrenIncreased t1 == False
    print $ grandchildrenIncreased t2 == True

data BTree = Empty | Node Int BTree BTree

t1 :: BTree
t1 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty))

t2 :: BTree
t2 = Node 8 (Node 3 (Node 9 Empty Empty) (Node 10 Empty Empty)) (Node 10 (Node 11 Empty Empty) (Node 14 Empty Empty))

getLevel :: BTree -> Int -> [Int]
getLevel Empty _          = []
getLevel (Node v l r) 0 = [v]
getLevel (Node v l r) n = getLevel l (n - 1) ++ getLevel r (n - 1)

grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased Empty = True
grandchildrenIncreased (Node v l r) = all (> v) (getLevel l 1) && all (> v) (getLevel r 1) && grandchildrenIncreased l && grandchildrenIncreased r

