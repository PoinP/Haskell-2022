import Data.List

main :: IO()
main = do
    print $ isBinarySearchTree t1 == True
    print $ isBinarySearchTree t2 == False
    print $ isBinarySearchTree t3 == False

data BTree = Empty | Node Int BTree BTree

t1 :: BTree
t1 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty))

t2 :: BTree
t2 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 5 Empty Empty) (Node 14 Empty Empty))

t3 :: BTree
t3 = Node 8 (Node 3 (Node 5 Empty Empty) (Node 6 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty))

isBinarySearchTree :: BTree -> Bool
isBinarySearchTree t = tree == sort tree
    where
        tree = traverseTree t

traverseTree :: BTree -> [Int]
traverseTree Empty = []
traverseTree (Node v l r) = traverseTree l ++ [v] ++ traverseTree r