main :: IO()
main = do
    print $ cook [ApplePie, ApplePie, Burger, Chicken, Chicken, ApplePie] == [Sunny, Rainy, Rainy, Sunny, Rainy]
    print $ cook [ApplePie, Burger, Chicken, Chicken, ApplePie, Burger] == [Rainy , Rainy, Sunny, Rainy, Rainy]

data Dish = ApplePie | Burger | Chicken
    deriving (Eq)

data Weather = Sunny | Rainy
    deriving (Eq, Show)

cook :: [Dish] -> [Weather]
cook [] = []
cook [x] = []
cook (x:y:xs)
    | x == y    = Sunny : cook (y:xs)
    | otherwise = Rainy : cook (y:xs)