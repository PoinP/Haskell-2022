-- Define a function that returns all words contained
-- in all of the trees passed as a list.

-- Implementation detail: Use folding.

import Data.List

main :: IO()
main = do
    print $ allContain [t1, t2] == ["acd","cd","d"]
    print $ allContain [t1, t2, t3] == []
    print $ allContain [t3, t4] == ["g"]

data BTree a = Nil | Node Char (BTree a) (BTree a)

t1 :: BTree Char
t1 = Node 'a' (Node 'c' (Node 'f' Nil Nil) (Node 'd' Nil Nil)) (Node 'b' Nil (Node 'e' Nil Nil))

t2 :: BTree Char
t2 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'b' Nil Nil)

t3 :: BTree Char
t3 = Node 'a' (Node 'b' (Node 'd' (Node 'h' Nil Nil) (Node 'i' Nil Nil)) (Node 'e' Nil Nil)) (Node 'c' (Node 'f' Nil Nil) (Node 'g' Nil Nil)) 

t4 :: BTree Char
t4 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'g' Nil Nil)

allContain :: [BTree Char] -> [String]
allContain [] = []
allContain [t] = genWords t
allContain ts = foldr1 (\ws1 ws2 -> intersect ws1 ws2) ws
    where ws = map genWords ts

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