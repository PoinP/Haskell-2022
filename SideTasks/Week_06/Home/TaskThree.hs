-- By using the function normalize from last week, 
-- define the following functions for the Rat data type:

-- sumRats - returns the sum of two rational numbers
-- multiplyRats - returns the product of two rational numbers
-- divideRats - returns the quotient of two rational numbers
-- areEqual - returns whether two rational numbers are equal

main :: IO()
main = do
    print $ sumRats (2, 5) (5, 2) == (29, 10)
    print $ sumRats (52, 123) (96, 14) == (6268, 861)
    print $ sumRats (2, 5) (3, 5) == (1, 1)

    print $ multiplyRats (2, 5) (5, 2) == (1, 1)
    print $ multiplyRats (52, 123) (96, 14) == (832, 287)
    print $ multiplyRats (2, 5) (3, 5) == (6, 25)

    print $ divideRats (2, 5) (5, 2) == (4, 25)
    print $ divideRats (52, 123) (96, 14) == (91, 1476)
    print $ divideRats (2, 5) (3, 5) == (2, 3)

    print $ areEqual (2, 5) (5, 2) == False
    print $ areEqual (52, 123) (52 * 3, 123 * 3) == True
    print $ areEqual (2, 6) (5, 15) == True

type Rat = (Int, Int)

normalize :: Rat -> Rat
normalize (_, 0)   = error "Division by zero!"
normalize (x1, x2) = (div x1 d, div x2 d)
    where d = gcd x1 x2

sumRats :: Rat -> Rat -> Rat
sumRats (_, 0) _          = error "Division by zero!"
sumRats _ (_, 0)          = error "Division by zero!"
sumRats (x1, x2) (y1, y2) = normalize ((x1 * div m x2) + (y1 * div m y2), m)
    where m = lcm x2 y2

multiplyRats :: Rat -> Rat -> Rat
multiplyRats (_, 0) _          = error "Division by zero!"
multiplyRats _ (_, 0)          = error "Division by zero!"
multiplyRats (x1, x2) (y1, y2) = normalize (x1 * y1, x2 * y2)

divideRats :: Rat -> Rat -> Rat
divideRats (_, 0) _          = error "Division by zero!"
divideRats _ (_, 0)          = error "Division by zero!"
divideRats (x1, x2) (y1, y2) = multiplyRats (x1, x2) (y2, y1)

areEqual :: Rat -> Rat -> Bool
areEqual (x1, x2) (y1, y2) = normalize (x1, x2) == normalize (y1, y2)