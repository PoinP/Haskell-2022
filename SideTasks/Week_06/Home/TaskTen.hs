import Data.Char (digitToInt)
-- Define a function that accepts a number and returns the tuple (x, y)
-- where x is the sum of the digits on even indices of the number and 
-- y - the sum of the ones on odd indices.

-- Implementation detail: Solve the task with one line of code using folding.

main :: IO()
main = do
    print $ checkNumber 2728 == (4,15)
    print $ checkNumber 31415 == (12,2)
    print $ checkNumber 121 == (2,2)

getCorrectTuple :: (Int, Int, Int) -> (Int, Int)
getCorrectTuple (x, y, _) = (x, y)

checkNumber :: Int -> (Int, Int)
checkNumber = getCorrectTuple . foldl (\(a, b, c) x -> if even c then (a + x, b, c + 1) else (a, b + x, c + 1)) (0, 0, 0) . map digitToInt . show

-- Couldn't really do it in one line :(