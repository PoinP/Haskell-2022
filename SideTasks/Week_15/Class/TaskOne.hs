main :: IO()
main = do
    print $ squareDigits 9119  == 811181
    print $ squareDigits (-9119) == -811181

squareDigitsList :: Int -> [String]
squareDigitsList 0 = []
squareDigitsList n = show (mod (abs n) 10 ^ 2) : squareDigitsList (div (abs n) 10)

squareDigits :: Int -> Int
squareDigits n
    | n < 0 = (-1) * read (concat $ squareDigitsList n)
    | otherwise = read $ concat $ squareDigitsList n