-- Define a function that returns the depth 
-- of the shallowest green node.

main :: IO()
main = do
    print $ minDepthGreenNode colorTree == 2

data Color = Red | Green | Blue
    deriving (Eq)
data Tree = Empty | Node Color Tree Tree

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

minDepthGreenNode :: Tree -> Int
minDepthGreenNode t = minimum $ acc t 0
    where
        acc :: Tree -> Int -> [Int]
        acc Empty _ = []
        acc (Node Green l r) n = n : acc l (n + 1) ++ acc r (n + 1)
        acc (Node _ l r) n = acc l (n + 1) ++ acc r (n + 1)
