main :: IO()
main = do
    -- print $ (kthNumber [-2,3,5,-4,-13,-15,20,-21] (> 5)) 12
    print $ (kthNumber [-2,3,5,-4,-13,-15,20,-21] (>= 5)) 2 == 20

kthNumber :: [Int] -> (Int -> Bool) -> (Int -> Int)
kthNumber xs p = (\k -> if length numbers < k then error "No such number" else numbers !! (k - 1))
    where
        numbers = [x | x <- xs, p x]