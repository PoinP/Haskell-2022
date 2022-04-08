-- Define the function sine, that accepts a whole number and a real 
-- number (representing radians) and returns the n-th partial sum of sin(x).

main :: IO()
main = do
   print $ mySin 100 1 == 0.8414709848078965 -- n = 100, x = 1
   print $ mySin 100 0.5 == 0.479425538604203

fact :: Integer -> Integer
fact 0 = 1
fact x = x * fact (x - 1)

calc :: Double -> Integer -> Double 
calc x i = (-1)^i * (x^(2*i + 1)) /  fromIntegral(fact(2*i + 1))

mySin :: Integer -> Double -> Double 
mySin 0 x = x
mySin n x = calc x n + mySin (n - 1) x