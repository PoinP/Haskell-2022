-- Define a function that returns the number of occurrences of a given digit in a given positive number.

main :: IO()
main = do
    print $ countOccurences 121 1 == 2
    print $ countOccurences 1252212 2 == 4

countOccurences :: Int -> Int -> Int 
countOccurences x srch = acc x 0
    where
        acc :: Int -> Int -> Int
        acc 0 occ = occ
        acc n occ
            | mod n 10 == srch = acc (div n 10) (occ + 1)
            | otherwise        = acc (div n 10) occ