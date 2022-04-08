import Data.Char

-- Define a function that accepts a string and removes all duplicate letters
-- Note:
-- Two characters are duplicate, if:
--     - they represent the same character;
--     - they are next to each other;
--     - the first is upper-case and the second - lower-case (or vice versa).

main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
    -- Correct output?? - "dabACaCBAcaDD"

-- Why are the letters A(a) removed when they are not next to each other?

reduceStr :: [Char] -> [Char]
reduceStr [] = []
reduceStr [c] = [c]
reduceStr (c:str)
    | isUpper c && isLower (str !! 0) && c == toUpper (str !! 0) = reduceStr str
    | isLower c && isUpper (str !! 0) && str !! 0 == toUpper c   = reduceStr str
    | otherwise                                                  = c : reduceStr str 