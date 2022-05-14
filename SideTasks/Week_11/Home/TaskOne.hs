-- For the trees in task 1 (that we solved in class) define the following functions:

-- ​​height (number of nodes along the longest branch);​
-- average - returns the average of the nodes (should work only for trees that store numbers in their nodes);
-- ​​sumLeaves​ - returns the sum of the leaves (should work only for trees that store numbers in their nodes);
-- areEqual - checks whether two trees are identical;
-- setLevels - replaces the values in all nodes with the vector ("level", "value");
-- mirrorTree - returns the symmetric tree.

main :: IO()
main = do
    print $ height numberBTree == 4
    print $ height charBTree == 3

    print $ average numberBTree == 16.22222222222222
    --print $ average charBTree -- should not work

    print $ sumLeaves numberBTree == 119
    -- print $ sumLeaves charBTree -- shouldn't work

    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil Nil))) == False
    print $ areEqual charBTree charBTree == True
    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))) == False

    print $ setLevels numberBTree == Node (0,5) (Node (1,12) (Node (2,1) (Node (3,96) Nil Nil) Nil) (Node (2,0) Nil Nil)) (Node (1,4) (Node (2,2) Nil Nil) (Node (2,5) Nil (Node (3,21) Nil Nil)))
    print $ setLevels charBTree == Node (0,'k') (Node (1,'a') (Node (2,'h') Nil Nil) (Node (2,'s') Nil Nil)) (Node (1,'l') (Node (2,'e') Nil Nil) (Node (2,'l') Nil Nil))

    print $ mirrorTree numberBTree == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
    print $ mirrorTree charBTree == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a)
    deriving (Show, Eq)

numberBTree :: BTree Int
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))

height :: BTree a -> Int
height Nil = 0
height (Node v l r) = max (treeSize l) (treeSize r)

average :: BTree Int -> Double
average t = fromIntegral (treeSum t) / fromIntegral (treeSize t)

sumLeaves :: BTree Int -> Int
sumLeaves Nil = 0
sumLeaves (Node v Nil Nil) = v
sumLeaves (Node v l r) = sumLeaves l + sumLeaves r

areEqual :: (Eq a) => BTree a -> BTree a -> Bool
areEqual t1 t2 = traverseTree t1 == traverseTree t2

setLevels :: BTree a -> BTree (Int, a)
setLevels t = acc t 0
    where
        acc Nil n = Nil
        acc (Node v l r) n = Node (n, v) (acc l (n + 1)) (acc r (n + 1))

mirrorTree :: BTree a -> BTree a
mirrorTree Nil = Nil
mirrorTree (Node v l r) = Node v (mirrorTree r) (mirrorTree l)


traverseTree :: BTree a -> [a]
traverseTree Nil = []
traverseTree (Node v l r) = v : traverseTree l ++ traverseTree r

treeSize :: BTree a -> Int
treeSize Nil = 0
treeSize (Node v l r) = 1 + treeSize l + treeSize r

treeSum :: BTree Int -> Int
treeSum Nil = 0
treeSum (Node v l r) = v + treeSum l + treeSum r