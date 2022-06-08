import Data.List
import Data.Char (digitToInt)

main :: IO()
main = do
    print $ digits 4321 == [4,3,2,1]
    print $ isDecreasing [4,3,2,1] == True
    print $ isDecreasing [4,3,5,1] == False
    print $ isDecreasing [4,3,3,1] == False


digits :: Int -> [Int]
digits = reverse . sort . map digitToInt . show

isDecreasing :: [Int] -> Bool
isDecreasing xs = and [x1 > x2 | (x1, x2) <- zip xs (tail xs)]

intToList :: Int -> [Int]
intToList 0 = []
intToList n = mod n 10 : intToList(div n 10)