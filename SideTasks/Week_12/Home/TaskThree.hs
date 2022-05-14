-- Define a function that returns the depth of the deepest blue node.

-- Use the following types:

-- data Color = Red | Green | Blue
-- data Tree = Empty | Node Color Tree Tree

main :: IO()
main = do
    print $ maxDepthBlueNode colorTree == 2

data Color = Red | Green | Blue
    deriving (Eq)
data Tree = Empty | Node Color Tree Tree

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

getLevel :: Tree -> Int -> [Color]
getLevel Empty _ = []
getLevel (Node c l r) 0 = [c]
getLevel (Node c l r) n = getLevel l (n - 1) ++ getLevel r (n - 1)

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode Empty = 0
maxDepthBlueNode t = acc 0 0
    where 
        acc l d
            | elem Blue (getLevel t l) = acc (l + 1) l
            | null $ getLevel t l = d
            | otherwise = acc (l + 1) d