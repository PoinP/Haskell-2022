main :: IO()
main = do
    print $ dominates (+4) (*2) [1 .. 4] == True
    print $ dominates (+4) (*2) [1 .. 5] == False

dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g xs = and [abs (f x) >= abs (g x) | x <- xs]