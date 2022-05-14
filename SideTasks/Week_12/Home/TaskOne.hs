-- Define a function that accepts an infinite list of 
-- numbers [x1, x2 .. ] and returns a function that for 
-- every x and y calculates the expression (x - x1)(x - x2) .. (x - xy).

main :: IO()
main  = do
    print $ myPoly [2.7, 3.0 ..] 2.2 3 == -0.4399999999999998

myPoly :: [Double] -> (Double -> Int -> Double)
myPoly xs x y = product $ map (x -) (take y xs)