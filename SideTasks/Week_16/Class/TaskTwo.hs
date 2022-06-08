import Data.Char

main :: IO()
main = do
    print $ (speak "gate") 't' == "ga1e"
    print $ (speak "This is a test") 'i' == "Th11s 8s a test"
    print $ (speak "iiiiiii") 'i' == "6543210"
    print $ (speak "This is another test that has more words") 'a' == "This is 31nother test th16t h12s more words"

speak :: String -> (Char -> String)
speak str = (\c -> foldr (\(el, pos) acc -> if (el == c) then (if (isDigit' pos) then [intToDigit pos] else intToNumber pos) ++ acc else el : acc) [] tuple)
    where 
        tuple :: [(Char, Int)]
        tuple = [(el, pos) | (el, pos) <- zip str (reverse [0 .. length str - 1])]

isDigit' :: Int -> Bool
isDigit' d = div d 10 == 0

intToNumber :: Int -> String
intToNumber 0 = []
intToNumber n = intToNumber (div n 10) ++ [intToDigit (mod n 10)]