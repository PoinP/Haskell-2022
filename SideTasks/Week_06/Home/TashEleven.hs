-- Write a function that, for a list xss
-- whose elements are non-empty lists of 
-- numbers, returns a list of those elements 
-- of xss that represent an arithmetic progression.

main :: IO()
main = do
    print $ onlyArithmetic [[3], [1, 2, 3, 4, 5], [3, 5, 8, 9, 11]]  == [[3], [1, 2, 3, 4, 5]]

isArithmetic :: [Int] -> Bool
isArithmetic [] = False
isArithmetic [x] = True
isArithmetic xs = xs == take (length xs) [xs !! 0, xs !! 1 ..]

onlyArithmetic :: [[Int]] -> [[Int]]
onlyArithmetic xss = [xs | xs <- xss, isArithmetic xs]
