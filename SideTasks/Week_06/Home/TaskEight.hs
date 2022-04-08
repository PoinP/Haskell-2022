import Data.List
import Data.Char

-- Write a function returns the count of distinct 
-- case-insensitive alphabetic characters and numeric 
-- digits that occur more than once in the input string. 
-- The input string can be assumed to contain only alphabets 
-- (both uppercase and lowercase) and numeric digits.

main :: IO()
main = do
    print $ duplicateCount "" == 0 -- no characters repeats more than once
    print $ duplicateCount "abcde" == 0
    print $ duplicateCount "aabbcde" == 2 -- 'a' and 'b'
    print $ duplicateCount "aabBcde" == 2 -- 'a' occurs twice and 'b' twice (`b` and `B`)
    print $ duplicateCount "indivisibility" == 1 -- 'i' occurs six times
    print $ duplicateCount "Indivisibility" == 1
    print $ duplicateCount "aA11" == 2 -- 'a' and '1'
    print $ duplicateCount "ABBA" == 2 -- 'A' and 'B' each occur twice
    print $ duplicateCount "Indivisibilities" == 2 -- 'i' occurs seven times and 's' occurs twice
    print $ duplicateCount ['a'..'z'] == 0
    print $ duplicateCount (['a'..'z'] ++ ['A'..'Z']) == 26

duplicateCount :: [Char] -> Int
duplicateCount str = length $ filter (\str -> length str > 1) $ group $ sort [toLower c | c <- str]
-- I am also kind of proud of this one too!