-- Define numberTreeBefore using a constructor.

-- Define a function that inserts a node in a binary search tree. 
-- By using it construct the same tree and check their equality.

-- Define numberTreeBefore (name the new tree numberTreeAfter) 
-- and secondNumberTree using insert.

main :: IO()
main = do
    print $ insert (Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 Nil (Node 18 Nil Nil))) 13 == (Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil)))

    print $ (numberTreeAfter == numberTreeBefore) == True

    print $ secondNumberTree == Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a) 
    deriving (Show, Eq)

numberTreeBefore :: BTree Int
numberTreeBefore = Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 Nil (Node 18 Nil Nil))

numberTreeAfter :: BTree Int
numberTreeAfter = foldl (\acc n -> insert acc n) Nil [10, 5, 3, 7, 15, 18]

secondNumberTree :: BTree Int
secondNumberTree = foldl insert Nil [10, 5, 3, 1, 7, 6, 15, 13, 18]

insert :: (Ord a) => BTree a -> a -> BTree a
insert Nil k = Node k Nil Nil
insert (Node v l r) k
    | k > v     = Node v l (insert r k)
    | otherwise = Node v (insert l k) r
