-- A snail crawls up a column. During the day it crawls up some distance. 
-- During the night it sleeps, so it slides down for some distance 
-- (less than it crawls up during the day).

-- Your function accepts three arguments:
--      The height of the column (meters);
--      The distance that the snail crawls during the day (meters);
--      The distance that the snail slides down during the night (meters).

-- Calculate the number of days the snail will need to reach the top of the column.

main :: IO()
main = do
    print $ snail 3 2 1 == 2
    print $ snail 10 3 1 == 5
    print $ snail 10 3 2 == 8
    print $ snail 100 20 5 == 7
    print $ snail 5 10 3 == 1

snail :: Int -> Int -> Int -> Int
snail desiredHeight upSpeed downSpeed = 
    if downSpeed > upSpeed
        then error "The snake can not slide down more that it can crawl up!"
        else acc upSpeed 1 where
        acc :: Int -> Int -> Int
        acc x d
            | x >= desiredHeight = d
            | otherwise          = acc (x + upSpeed - downSpeed) (d + 1)