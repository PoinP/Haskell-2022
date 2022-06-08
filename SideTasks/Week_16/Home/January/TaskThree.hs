main :: IO()
main = do
    print $ splitPoints (1, 1) 5 [(1, 2), (2, 3), (10, 15), (-1, 1), (12, 14)] == ([(1.0, 2.0), (2.0, 3.0), (-1.0, 1.0)], [(10.0, 15.0), (12.0, 14.0)])

type Point = (Double, Double)

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints p r ps = (filter (\point -> isInCircle p r point) ps, filter (\point -> not $ isInCircle p r point) ps)

isInCircle :: Point -> Double -> Point -> Bool
isInCircle (x1, y1) r (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2) <= r