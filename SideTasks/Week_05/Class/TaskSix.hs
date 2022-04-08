-- Define the Rat data type that is the tuple (x, y) 
-- where x is the numerator and y - the denominator 
-- of a rational number. Define a function called
-- normalize which will simplify the numerator and 
-- denominator (so that their highest common denominator is 1).

-- Implementation detail: Use the type annotation!

main :: IO()
main = do
    print $ normalize (4, 2) == (2, 1)
    print $ normalize (8, 4) == (2, 1)
    print $ normalize (2, 4) == (1, 2)

    print $ normalizeUsingLet (4, 2) == (2, 1)
    print $ normalizeUsingLet (8, 4) == (2, 1)
    print $ normalizeUsingLet (2, 4) == (1, 2)

type Rat = (Int, Int)

normalize :: Rat -> Rat
normalize (x, y) = (div x d, div y d)
    where
        d = gcd x y

normalizeUsingLet :: Rat -> Rat
normalizeUsingLet (x, y) = let d = gcd x y
    in
        (div x d, div y d)