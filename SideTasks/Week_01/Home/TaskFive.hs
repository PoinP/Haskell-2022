-- Define two predicates that check whether a year is a leap year.

-- The first should be called leap-year-one-line?. It should implement Method I using boolean logical operators.

-- The second should be called is-leap-year-guards?. It should implement Method II using guards.

main :: IO()
main = do
    print $ isLeapYearOneLine 2020 == True
    print $ isLeapYearOneLine 1988 == True
    print $ isLeapYearOneLine 1600 == True
    print $ isLeapYearOneLine 2400 == True
    print $ isLeapYearOneLine 2023 == False
    print $ isLeapYearOneLine 1700 == False
    print $ isLeapYearOneLine 1800 == False
    print $ isLeapYearOneLine 2100 == False

    print $ isLeapYearGuards 2020 == True
    print $ isLeapYearGuards 1988 == True
    print $ isLeapYearGuards 1600 == True
    print $ isLeapYearGuards 2400 == True
    print $ isLeapYearGuards 2023 == False
    print $ isLeapYearGuards 1700 == False
    print $ isLeapYearGuards 1800 == False
    print $ isLeapYearGuards 2100 == False

isLeapYearOneLine :: Int -> Bool
isLeapYearOneLine x = mod x 400 == 0 ||  mod x 100 /= 0 && mod x 4 == 0

isLeapYearGuards :: Int -> Bool
isLeapYearGuards x
    | mod x 400 == 0                 = True
    | mod x 100 /= 0 && mod x 4 == 0 = True
    | otherwise                      = False