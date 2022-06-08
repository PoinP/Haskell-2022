main :: IO()
main = do
    print $ isBalanced t1  == False
    print $ isBalanced t2  == True

data BTree = Empty | Node Int BTree BTree
    deriving(Eq, Show)

t1 :: BTree
t1 = Node 5 Empty (Node 4 (Node 5 Empty Empty) (Node 7 Empty Empty))

t2 :: BTree
t2 = Node 5 (Node 3 Empty Empty) (Node 4 (Node 5 Empty Empty) (Node 7 Empty Empty))

isBalanced :: BTree -> Bool
isBalanced Empty = True
isBalanced (Node _ l r) = abs (left - right) <= 1 && isBalanced l && isBalanced r
    where
        left = getLength l
        right = getLength r

getLength :: BTree -> Int
getLength Empty = 0
getLength (Node v l r) = 1 + max (getLength l) (getLength r)