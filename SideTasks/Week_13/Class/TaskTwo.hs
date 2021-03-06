import Data.List

main :: IO()
main = do
    print $ genWords t1 == ["abe","acd","acf","be","cd","cf","d","e","f"]
    print $ genWords t2 == ["ab","acd","b","cd","d"]
    print $ genWords t3 == ["abdh","abdi","abe","acf","acg","bdh","bdi","be","cf","cg","dh","di","e","f","g","h","i"]

data BTree a = Nil | Node Char (BTree a) (BTree a)

t1 :: BTree Char
t1 = Node 'a' (Node 'c' (Node 'f' Nil Nil) (Node 'd' Nil Nil)) (Node 'b' Nil (Node 'e' Nil Nil))

t2 :: BTree Char
t2 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'b' Nil Nil)

t3 :: BTree Char
t3 = Node 'a' (Node 'b' (Node 'd' (Node 'h' Nil Nil) (Node 'i' Nil Nil)) (Node 'e' Nil Nil)) (Node 'c' (Node 'f' Nil Nil) (Node 'g' Nil Nil))

genWordsFromRoot :: BTree Char -> [String]
genWordsFromRoot t = acc t []
    where
        acc :: BTree Char -> [String] -> [String]
        acc Nil str            = str
        acc (Node c l Nil) str = acc l [concat (str ++ [[c]])]
        acc (Node c Nil r) str = acc r [concat (str ++ [[c]])]
        acc (Node c l r)   str = acc l [concat (str ++ [[c]])] ++ acc r [concat (str ++ [[c]])]

genWords :: BTree Char -> [String]
genWords Nil = []
genWords t@(Node c l r) = sort $ nub $ genWordsFromRoot t ++ genWordsFromRoot l ++ genWordsFromRoot r ++ genWords l ++ genWords r


--- Second version

-- containsWord :: BTree Char -> [Char] -> Bool
-- containsWord Nil _ = False
-- containsWord _ []  = False
-- containsWord (Node c Nil Nil) [x] = x == c
-- containsWord (Node c l r) word@(x:xs)
--     | x /= c = containsWord l word || containsWord r word
--     | otherwise = acc l xs || acc r xs
--         where 
--             acc Nil _ = False
--             acc _ []  = False
--             acc (Node c Nil Nil) [x] = c == x
--             acc (Node c l r) (x:str) = c == x && (acc l str || acc r str)

-- traverseDFS :: BTree Char -> [[Char]]
-- traverseDFS Nil = []
-- traverseDFS (Node v l r) = filter containsWord $ nub $ concatMap tails $ map (v:) (traverseDFS l) ++ [[v]] ++ map (v:) (traverseDFS r)

-- Third Method