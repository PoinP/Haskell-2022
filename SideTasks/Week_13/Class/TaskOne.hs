-- Define a function that checks whether a word is present in a binary tree made up of characters.

main :: IO()
main = do
    print $ containsWord t1 "acd" == True
    print $ containsWord t1 "cd" == True
    print $ containsWord t1 "af" == False
    print $ containsWord t1 "ac" == False
    print $ containsWord t1 "acdh" == False
    print $ containsWord t1 "b" == False
    print $ containsWord t1 "e" == True
    print $ containsWord t2 "ab" == True
    print $ containsWord t2 "ad" == False
    print $ containsWord t3 "bdh" == True
    print $ containsWord t3 "bdi" == True
    print $ containsWord t3 "ac" == False

data BTree a = Nil | Node Char (BTree a) (BTree a)

t1 :: BTree Char
t1 = Node 'a' (Node 'c' (Node 'f' Nil Nil) (Node 'd' Nil Nil)) (Node 'b' Nil (Node 'e' Nil Nil))

t2 :: BTree Char
t2 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'b' Nil Nil)

t3 :: BTree Char
t3 = Node 'a' (Node 'b' (Node 'd' (Node 'h' Nil Nil) (Node 'i' Nil Nil)) (Node 'e' Nil Nil)) (Node 'c' (Node 'f' Nil Nil) (Node 'g' Nil Nil))

-- containsWord :: BTree Char -> [Char] -> Bool
-- containsWord _ [] = True
-- containsWord Nil _ = False
-- containsWord (Node c' l r) (c:str) = c == c' && (containsWord l str || containsWord r str)


containsWord :: BTree Char -> [Char] -> Bool
containsWord Nil _ = False
containsWord _ []  = False
containsWord (Node c Nil Nil) [x] = x == c
containsWord (Node c l r) word@(x:xs)
    | x /= c = containsWord l word || containsWord r word
    | otherwise = acc l xs || acc r xs
        where 
            acc Nil _ = False
            acc _ []  = False
            acc (Node c Nil Nil) [x] = c == x
            acc (Node c l r) (x:str) = c == x && (acc l str || acc r str)