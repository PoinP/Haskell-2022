-- Define a function that takes a single argument function and returns it applied n times.

main :: IO()
main = do
    print $ (applyN (\x -> 2 * x) 5) 2 == 64
    print $ (applyN (\x -> div x 10) 2) 100 == 1

applyN :: (Int -> Int) -> Int -> Int -> Int
applyN f 1 x = f x
applyN f n x = applyN f (n - 1) (f x)