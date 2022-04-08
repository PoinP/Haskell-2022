-- Define a function that accepts two numbers - 
-- s and f (s < f) and a step k and returns a list 
-- of tuples (x, y) where y is the square of x.

-- Note: x goes from s to f with a step k.

main :: IO()
main = do
    print $ getSquares 0 100 10 == [(0, 0), (10, 100), (20, 400), (30, 900), (40, 1600), (50, 2500), (60, 3600), (70, 4900), (80, 6400), (90, 8100), (100, 10000)]

getSquares :: Int -> Int -> Int -> [(Int, Int)]
getSquares s f k
    | s > f     = []
    | s == f    = [(s, s ^ 2)]
    | otherwise = (s, s ^ 2) : getSquares (s + k) f k

getSquares' :: Int -> Int -> Int -> [(Int, Int)]
getSquares' s f k = [ (x, x ^ 2) | x <- [s, s + k .. f] ]