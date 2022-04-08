-- Define a function on a functional level that takes a word 
-- and returns a list of tuples in the form (x, xCount) where 
-- for each letter x, xCount is the number of times it is present 
-- in the word. Ignore capitalization.

import Data.Char
import Data.List

main :: IO()
main = do
    print $ countOccurrences "Test" == [('e',1),('s',1),('t',2)]
    print $ countOccurrences "ThisIsAReallyLongWordContaingAlmostEveryCharacter" == [('a',6),('c',3),('d',1),('e',4),('g',2),('h',2),('i',3),('l',4),('m',1),('n',3),('o',4),('r',5),('s',3),('t',4),('v',1),('w',1),('y',2)]

timesSeen :: Char -> [Char] -> Int
timesSeen c [] = 0
timesSeen c (s:str)
    | c == s    = 1 + timesSeen c str
    | otherwise = timesSeen c str

countEachLetter :: [Char] -> [(Char, Int)]
countEachLetter str = let letters = ['a' .. 'z']
    in
        [(n, timesSeen n str) | n <- letters, timesSeen n str > 0 ]

countOccurrences :: [Char] -> [(Char, Int)]
countOccurrences str = countEachLetter (map toLower str)

countOccurrences' :: String -> [(Char, Int)]
countOccurrences' = map (\xs -> (head xs, length xs)) . group . sort . map toLower 
-- How does this even work??!!!??!!? This is very smart
-- Looked up how group works and it really is black magic haha