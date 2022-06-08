main :: IO()
main = do
    print $ isPerfectlyBalanced t == True

data BTree a = Nil | Node a (BTree a) (BTree a)

t :: BTree Char
t = Node 'H' (Node 'a' (Node 'k' Nil Nil) (Node 'e' Nil Nil)) (Node 's' (Node 'l' Nil Nil) (Node 'l' Nil Nil))

isPerfectlyBalanced :: BTree a -> Bool
isPerfectlyBalanced t = length (traverseTree t) == 2^size - 1
    where size = getSize t

getSize :: BTree a -> Int
getSize Nil = 0
getSize (Node v l r) = 1 + max (getSize l) (getSize r)

traverseTree :: BTree a -> [a]
traverseTree Nil = []
traverseTree (Node v l r) = v : traverseTree l ++ traverseTree r