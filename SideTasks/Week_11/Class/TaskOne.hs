import Data.Char

-- Define a recursive polymorphic algebraic type called BTree. 
-- Create the following instances and print them:

-- Define the following functions:

-- ​​size - returns the number of nodes;
-- sumTree - returns the sum of the nodes (should work only for trees that store numbers in their nodes);​
-- traverseDFS​ - prints the nodes using DFS;
-- getLevel - accepts a whole number k and returns the nodes at level k (root is at level 0);
-- traverseBFS - prints the nodes using BFS;
-- mapTree - maps an unary function to the tree.

main :: IO()
main = do
    print $ numberBTree
    print $ charBTree

    print $ size numberBTree == 9
    print $ size charBTree == 7

    print $ sumTree numberBTree == 146
    -- print $ sumTree charBTree -- should not work

    print $ traverseDFS numberBTree == [96, 1, 12, 0, 5, 2, 4, 5, 21]
    print $ traverseDFS charBTree == "haskell"

    print $ getLevel numberBTree 2 == [1, 0, 2, 5]
    print $ getLevel charBTree 1 == "al"
    print $ getLevel charBTree 3 == []

    print $ traverseBFS numberBTree == [5,12,4,1,0,2,5,96,21]
    print $ traverseBFS charBTree == "kalhsel"

    print $ mapTree numberBTree (*2) == Node 10 (Node 24 (Node 2 (Node 192 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 4 Nil Nil) (Node 10 Nil (Node 42 Nil Nil)))
    print $ mapTree numberBTree (show) == Node "5" (Node "12" (Node "1" (Node "96" Nil Nil) Nil) (Node "0" Nil Nil)) (Node "4" (Node "2" Nil Nil) (Node "5" Nil (Node "21" Nil Nil)))
    print $ mapTree charBTree (toUpper) == Node 'K' (Node 'A' (Node 'H' Nil Nil) (Node 'S' Nil Nil)) (Node 'L' (Node 'E' Nil Nil) (Node 'L' Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a)
    deriving (Show, Eq)

numberBTree :: BTree Int
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))

size :: BTree a -> Int
size Nil          = 0
size (Node _ l r) = 1 + size l + size r

sumTree :: BTree Int -> Int
sumTree Nil          = 0
sumTree (Node v l r) = v + sumTree l + sumTree r

traverseDFS :: BTree a -> [a]
traverseDFS Nil          = []
traverseDFS (Node v l r) = traverseDFS l ++ [v] ++ traverseDFS r

getLevel :: BTree a -> Int -> [a]
getLevel Nil _          = []
getLevel (Node v l r) 0 = [v]
getLevel (Node v l r) k = getLevel l (k - 1) ++ getLevel r (k - 1)

traverseBFS :: (Eq a) => BTree a -> [a]
traverseBFS t = concat $ takeWhile (/= []) [getLevel t n | n <- [0 ..]]

mapTree :: BTree a -> (a -> b) -> BTree b
mapTree Nil _ = Nil
mapTree (Node v l r) f = Node (f v) (mapTree l f) (mapTree r f)