-- Each day a plant is growing by upSpeed meters. 
-- Each night that plants height decreases by 
-- downSpeed meters due to the lack of sun light. 
-- Initially, plant is 0 meters tall. We plant the 
-- seed at the beginning of a day. We want to know when 
-- the height of the plant will reach a certain level.

main :: IO()
main = do
    print $ growingPlant 5 2 5 == 1
    print $ growingPlant 5 2 6 == 2
    print $ growingPlant 10 9 4 == 1
    print $ growingPlant 100 10 910 == 10 -- upSpeed=100, downSpeed=10, desiredHeight=910

growingPlant :: Int -> Int -> Int -> Int
growingPlant upSpeed downSpeed desiredHeight = 
    acc upSpeed 1 where
        acc :: Int -> Int -> Int
        acc x d
            | x >= desiredHeight = d
            | otherwise          = acc (x + upSpeed - downSpeed) (d + 1)