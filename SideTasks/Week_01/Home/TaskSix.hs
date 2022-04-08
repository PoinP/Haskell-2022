-- John has a backpack. With it he can carry k kilograms. An item from the supermarket weighs w kilograms.

-- Define a predicate that accepts three numbers - c (number of products), 
-- k and w and returns whether John is capable of buying all the products in one trip to the supermarket.

-- Implementation detail:
-- Use guards.

main :: IO()
main = do
    print $ canCarry 5 15 3 == "Yes"
    print $ canCarry 1 5 4 == "Yes"
    print $ canCarry 13 25 2 == "No"
    print $ canCarry 24 104.44 21.12 == "No"
    print $ canCarry 51 34.75 19.852 == "No"
    print $ canCarry 42 95.11 0.51 == "Yes"

    --print $ canCarry (-13) 25 2 -- error: The number of products was negative
    --print $ canCarry 13 (-25) 2 -- error: John's hosting capacity was negative
    --print $ canCarry 13 25 (-2) -- error: The weight of a product was negative

canCarry :: Double -> Double -> Double -> [Char]
canCarry c k w
    | c < 0      = error "The number of products was negative"
    | k < 0      = error "John's hosting capacity was negative"
    | w < 0      = error "The weight of a product was negative"
    | w * c <= k = "Yes"
    | otherwise  = "No"