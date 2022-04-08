-- Define two predicates that return whether a number is even:

--      using if-else;
--      using guards. <- The preferred way!

main :: IO()
main = do
    print $ isEvenIf 2 == "Yes"
    print $ isEvenIf 15452 == "Yes"
    print $ isEvenIf 321 == "No"

    print $ isEvenGuards 2 == "Yes"
    print $ isEvenGuards 15452 == "Yes"
    print $ isEvenGuards 321 == "No"

isEvenIf :: Int -> [Char]
isEvenIf x =
    if mod x 2 == 0
        then "Yes"
        else "No"

isEvenGuards :: Int -> [Char]
isEvenGuards x
    | mod x 2 == 0 = "Yes"
    | otherwise = "No"