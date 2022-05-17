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

reduceStr :: [Char] -> [Char]
reduceStr str = acc str []
    where
        acc :: [Char] -> [Char] -> [Char]
        acc []  newStr = newStr
        acc [c] newStr = newStr ++ [c]
        acc (c:str) newStr
            | isUpper c && isLower (str !! 0) && c == toUpper (str !! 0)       = acc (tail str) newStr
            | isLower c && isUpper (str !! 0) && str !! 0 == toUpper c         = acc (tail str) newStr
            | null newStr                                                      = acc str (newStr ++ [c])
            | isUpper c && isLower (last newStr) && c == toUpper (last newStr) = acc str (init newStr)
            | isLower c && isUpper (last newStr) && (last newStr) == toUpper c = acc str (init newStr)
            | otherwise                                                        = acc str (newStr ++ [c])

-- My old function that inspired me to do the above one!

-- reduceStr :: [Char] -> [Char]
-- reduceStr [] = []
-- reduceStr [c] = [c]
-- reduceStr (c:str)
--     | isUpper c && isLower (str !! 0) && c == toUpper (str !! 0) = reduceStr (tail str)
--     | isLower c && isUpper (str !! 0) && str !! 0 == toUpper c   = reduceStr (tail str)
--     | otherwise                                                  = c : reduceStr str