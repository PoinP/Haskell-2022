-- A number - x, is a pentagonal number if 
-- we can plot x points in the form of a pentagon on a plain.

-- Define a function that accepts a natural number - n, and returns the n-th pentagonal number.

-- Implementation detail: Create a linearly iterative process.

main :: IO()
main = do
    print $ p 1 == 1
    print $ p 2 == 5
    print $ p 3 == 12
    print $ p 4 == 22
    print $ p 5 == 35
    print $ p 6 == 51

p :: Int -> Int
p x
    | x < 1 = error "Input Error: x must be greater than 0!"
    | otherwise = acc (x - 1) 3
        where
            acc 0 _ = 1
            acc 1 _ = 5
            acc x overlapping = x * 5 - overlapping + acc (x - 1) (overlapping + 2)


p'' :: Int -> Int
p'' 1 = 1
p'' 2 = 5
p'' x = createPentagon(x - 1) - overlapping 3 (x - 2) + p''(x - 1)

createPentagon :: Int -> Int
createPentagon x = x * 5

overlapping :: Int -> Int -> Int
overlapping x 1 = x
overlapping x n = overlapping (x + 2) (n - 1)


p' :: Int -> Int
p' 1 = 1
p' 2 = 5
p' x = (x - 1) * 5 - (3 + 2 * (x - 3)) + p'(x - 1) 