-- Given a list of whole numbers with no duplicates construct a 
-- maximum binary tree. We say that a binary tree is a maximum binary tree if:

-- The root is the maximum number in the list.
-- The left subtree is the maximum tree constructed from the elements on the left of the of the maximum number.
-- The right subtree is the maximum tree constructed from the elements on the right of the maximum number.

main :: IO()
main = do
    print $ constructMaxBTree [3, 2, 1, 6, 0, 5] == t

data BTree a = Nil | Node Int (BTree Int) (BTree Int)
    deriving (Show, Eq)

t :: BTree Int
t = Node 6 (Node 3 Nil (Node 2 Nil (Node 1 Nil Nil))) (Node 5 (Node 0 Nil Nil) Nil)

constructMaxBTree :: [Int] -> BTree Int
constructMaxBTree [] = Nil
constructMaxBTree xs = Node m (constructMaxBTree $ takeWhile (/=m) xs) (constructMaxBTree $ tail $ dropWhile (/=m) xs)
    where
        m = maximum xs