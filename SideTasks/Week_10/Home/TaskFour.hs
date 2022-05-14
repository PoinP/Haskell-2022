-- Define a function that returns all the uncles 
-- of a node in a tree. An uncle is a brother of a parent.

main :: IO()
main = do
    print $ findUncles t 5 == [3,4]
    print $ findUncles t 7 == [2,4]
    print $ findUncles t 10 == [5]

type Tree = [(Int, [Int])]

t :: Tree
t = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

findUncles :: Tree -> Int -> [Int]
findUncles t n = if (hasUncles t n) then filter (/= parent) $ head [cs | (p, cs) <- t, elem parent cs ] else []
        where parent = head $ [p | (p, cs) <- t, elem n cs]

hasUncles :: Tree -> Int -> Bool
hasUncles [] _ = False
hasUncles (x:t) n = notElem n (fst x : concat x)