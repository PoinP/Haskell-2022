-- Define a function that checks whether 
-- the digits of a non-negative number 
-- are ordered in non-decreasing order. 
-- The implementation should be on ONE line 
-- and must NOT include div or mod.

main :: IO()
main = do
    print $ isAscending 0 == True
    print $ isAscending 10 == False
    print $ isAscending 123 == True
    print $ isAscending 1233 == True
    print $ isAscending 12332 == False

isAscending :: Int -> Bool
isAscending n = n >= 0 && and [x <= y | (x, y) <- zip (show n) (tail $ show n)]

isAscending' :: Int -> Bool
isAscending' n = n >= 0 && and [x | x <- zipWith (<=) (show n) (tail $ show n)]