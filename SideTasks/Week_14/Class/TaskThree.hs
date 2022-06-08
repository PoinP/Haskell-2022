import Data.List

main :: IO()
main = do
    print $ isPrimeDictionary t1 vocabulary == False
    print $ isPrimeDictionary t2 vocabulary == False
    print $ isPrimeDictionary t3 vocabulary == True

type Vocabulary = [String]

data BTree = Nil | Node Char BTree BTree
 deriving (Show)
 
vocabulary :: Vocabulary
vocabulary = ["the", "a", "Some", "swimming", "liStS", "lisp"]

t1 :: BTree
t1 = Node 'a' (Node 't' (Node 'l' (Node 't' Nil Nil) (Node 'h' Nil Nil)) (Node 'i' (Node 'e' Nil Nil) (Node 'l' Nil Nil))) (Node 'h' (Node 's' (Node 'i' Nil Nil) (Node 'S' Nil Nil)) (Node 'a' (Node 't' Nil Nil) (Node 'S' Nil Nil)))

t2 :: BTree
t2 = Node 'a' (Node 't' (Node 'l' (Node 't' Nil Nil) (Node 'h' Nil Nil)) (Node 'i' (Node 'e' Nil Nil) (Node 'l' Nil Nil))) (Node 'h' (Node 's' (Node 'i' Nil Nil) (Node 's' Nil Nil)) (Node 'p' (Node 'p' Nil Nil) (Node 'S' Nil Nil)))

t3 :: BTree
t3 = Node 'a' (Node 't' (Node 'l' Nil Nil) (Node 'i' Nil Nil)) (Node 'h' (Node 's' Nil Nil) (Node 'p' Nil Nil))

isPrimeDictionary :: BTree -> Vocabulary -> Bool
isPrimeDictionary t v = isPrime $ getValues t v

isPrime :: Int -> Bool
isPrime n = null [x | x <- [2 .. n - 1], mod n x == 0]

getSize :: BTree -> Int
getSize Nil = 0
getSize (Node v l r) = 1 + max (getSize l) (getSize r)

getLevel :: BTree -> Int -> [Char]
getLevel Nil _ = []
getLevel (Node v _ _) 0 = [v]
getLevel (Node v l r) i = getLevel l (i - 1) ++ getLevel r (i - 1)

containsWord :: String -> [Char] -> Bool
containsWord str ws = or $ map (\w -> w == str) ((subsequences ws) ++ [ws])

getValues :: BTree -> Vocabulary -> Int
getValues t v = acc 0
    where
        acc l
            | l == getSize t + 1 = 0
            | otherwise = foldl (\acc w -> if containsWord w (getLevel t l) then acc + l + length w else acc ) 0 v + acc (l + 1)


