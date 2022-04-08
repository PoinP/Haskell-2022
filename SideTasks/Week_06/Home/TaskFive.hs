type Point = (Double, Double)

line :: Point -> Point -> (Double -> Double)
line (x1, y1) (x2, y2)
    | x1 == x2  = error "Input Error: x1 must be different from x2"
    | otherwise = (\x -> y1 + (x - x1) * (y2 - y1) / (x2 - x1))

-- liesOn :: (Double -> Double) -> (Point -> Bool)
-- liesOn f = (\x -> )

-- Can not understand how I can check if a point lies between two points
-- only by it's equation's resut :((