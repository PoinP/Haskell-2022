-- A binary tree is a cone if at every level the sum of the
-- nodes is greater than than the sum at the previous level.

-- For a binary tree made up of whole numbers define the 
-- following functions:

-- a function that returns the sum of the nodes at level k;
-- a function that returns whether a tree is a cone.

data BTree a = Nil | Node a (BTree a) (BTree a)

numberBTree :: BTree Int
numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True

levelSum :: (Num a) => BTree a -> Int -> a
levelSum Nil _ = 0
levelSum (Node v _ _) 0 = v
levelSum (Node v l r) n = levelSum l (n - 1) + levelSum r (n - 1)

cone :: (Num a, Ord a) => BTree a -> Bool
cone Nil              = True
cone (Node v Nil Nil) = True
cone t@(Node v l r)   = levelSum t 0 < levelSum t 1 && cone l && cone r 