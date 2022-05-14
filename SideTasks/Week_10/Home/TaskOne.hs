-- Define a function that accepts a unary function and a list 
-- of numbers [y1, y2 .. yn] and returns a function that accepts 
-- x and calculates the expression: y1f(x) + y2f(x2) + .. + ynf(xn).

main :: IO()
main = do
    print $ (sumExpr (2+) [0, 1, 2, 3]) 2 == 80
    print $ (sumExpr (*0.8) [0, 1, 2, 3, 4, 5]) 10 == 4345680.0

sumExpr :: Num a => (a -> a) -> [a] -> (a -> a)
sumExpr f ys = acc 1 ys
    where
        acc n []     = (\x -> x)
        acc n [y]    = (\x -> y * f (x ^ n))
        acc n (y:ys) = (\x -> y * f (x ^ n) + acc (n + 1) ys x)