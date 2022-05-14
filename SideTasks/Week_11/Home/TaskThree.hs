import Data.List

-- The nodes in the following picture resemple the lower and higher 
-- bounds of an interval (the first number will always be lower than the second). 
-- Define a function that checks whether such a binary tree is ordered according
-- to the relation subinterval.

main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False

data BTree a = Nil | Node (Int, Int) (BTree (Int, Int)) (BTree (Int, Int))
    deriving (Eq)

t1 :: BTree (Int, Int)
t1 = Node (3, 10) (Node (5,8) (Node (6, 7) Nil Nil) (Node (4, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

t2 :: BTree (Int, Int)
t2 = Node (3, 10) (Node (5,8) (Node (6, 7) Nil Nil) (Node (7, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

traverseTree :: BTree (Int, Int) -> [(Int, Int)]
traverseTree Nil = []
traverseTree (Node i l r) = traverseTree l ++ [i] ++ traverseTree r

ordered :: BTree (Int, Int) -> Bool
ordered Nil = True
ordered t@(Node v l r) = lNodes == reverse (sort lNodes) && rNodes == reverse (sort rNodes)
    where
        nodes = traverseTree t
        lNodes = takeWhile (/= v) nodes
        rNodes = tail $ dropWhile (/= v) nodes