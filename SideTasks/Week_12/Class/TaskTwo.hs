main :: IO()
main = do
    print $ numOfNodes t == 2 -- 8 and 12

data NTree a = Nil | Node a [NTree a]

t :: NTree Int
t = Node 10 [Node 3 [Node 5 [Nil], Node 8 [Node 1 [Nil], Node 2 [Nil]], Node 9 [Nil]], Node 7 [Node 11 [Nil], Node 13 [Nil]], Node 12 [Node 6 [Nil], Node 4 [Nil]]]

childrenSum :: [NTree Int] -> Int
childrenSum []               = 0
childrenSum (Nil:ns)         = 0
childrenSum ((Node v cs):ns) = v + childrenSum ns

getChildren :: NTree Int -> [NTree Int]
getChildren Nil         = []
getChildren (Node _ cs) = cs

numOfNodes :: NTree Int -> Int
numOfNodes Nil = 0
numOfNodes (Node v cs)
    | not $ null validNodes = length validNodes + sum (map numOfNodes cs)
    | otherwise             = sum $ map numOfNodes cs
        where validNodes = filter (\c -> childrenSum (getChildren c) == v) cs