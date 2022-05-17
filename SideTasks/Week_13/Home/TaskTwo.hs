-- Define a function that modifies a BST so that every node n has
-- a new value equal to the sum of the values of the original tree
-- that are greater than or equal to the value of n.

main :: IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))

data BTree a = Nil | Node Int (BTree Int) (BTree Int)
    deriving (Eq)

tree :: BTree Int
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

convert :: BTree Int -> BTree Int
convert t = acc t
    where
        acc Nil = Nil
        acc (Node v Nil Nil) = (Node (rangedSum t v maxBound :: Int) Nil Nil)
        acc (Node v l r)     = (Node (rangedSum t v maxBound :: Int) (acc l) (acc r))

rangedSum :: BTree Int -> Int -> Int -> Int
rangedSum Nil _ _ = 0
rangedSum (Node v l r) x y
    | l' <= v && r' >= v = rangedSum l l' r' + rangedSum r l' r' + v
    | otherwise          = rangedSum l l' r' + rangedSum r l' r'
        where 
            l' = min x y
            r' = max x y