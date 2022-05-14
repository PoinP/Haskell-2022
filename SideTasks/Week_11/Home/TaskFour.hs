-- For an algebraic type representing an n-ary tree,
-- define a predicate that checks whether it is a graceful tree.
-- We say that a tree is a graceful tree if the absolute difference
-- between every child node and its father is an even number.

main :: IO ()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False

data NTree a = Nil | Node a [NTree a]

t1 :: NTree Int
t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]

t2 :: NTree Int
t2 = Node 7 [Node 9 [Node 5 [Nil], Node 2 [Nil]]]

isEven :: Int -> Bool
isEven x = mod x 2 == 0

areChildrenGraceful :: Int -> [NTree Int] -> Bool
areChildrenGraceful _ []     = True
areChildrenGraceful v (Nil:xs) = True
areChildrenGraceful v cs@((Node v' _):cs') = isEven (abs (v - v')) && areChildrenGraceful v cs'

isGraceful :: NTree Int -> Bool
isGraceful Nil = True
isGraceful (Node v cs) = areChildrenGraceful v cs && all isGraceful cs