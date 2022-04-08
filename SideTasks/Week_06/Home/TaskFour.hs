-- Not gonna lie! Hardest one yet! 

main :: IO()
main = do
    print $ getAverageBalance (accounts1, people1) (\(_,_,city) -> city == "Burgas") == 24.95 -- 24.950000000000003??
    print $ getAverageBalance (accounts1, people1) (\(_,(n:_),_) -> n == 'P') == 18.85
    print $ getAverageBalance (accounts1, people1) (\ (n,_,_) -> n ==2) == 26.8

    print $ averageBalanceOfCities (accounts1,people1) ["Sofia","Gabrovo","Stara Zagora"] == 67.85
    print $ averageBalanceOfCities (accounts1, people1) ["Sofia"] == 67.85
    print $ averageBalanceOfCities (accounts1, people1) ["Burgas","Plovdiv"] == 23.62 -- 23.619999999999997
    print $ averageBalanceOfCities (accounts1,people1) ["Pleven", "Burgas", "Sofia","Gabrovo","Stara Zagora"] == 39.25 -- 39.24999999999999
    print $ averageBalanceOfCities (accounts1, people1) ["Sofia", "Gabrovo", "Burgas"] == 39.25 -- 39.24999999999999??

type Account = (Int, Int, Double)
type Person = (Int, String, String)

people1 :: [Person]
people1 = [(1, "Ivan", "Sofia"),(2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"),(4, "Petya", "Burgas")]

accounts1 :: [Account]
accounts1 = [(1, 1, 12.5),(2, 1, 123.2),(3, 2, 13.0),(4, 2, 50.2),(5, 2, 17.2),(6, 3, 18.3),(7, 4, 19.4)]

getBalance :: Account -> Double
getBalance (_, _, x) = x

getAccountPersonId :: Account -> Int
getAccountPersonId (_, x, _) = x

getPersonId :: Person -> Int
getPersonId (x, _, _) = x

getPersonCity :: Person -> String
getPersonCity (_, _, x) = x

getNeededBalanceData :: ([Account], [Person]) -> (Person -> Bool) -> [Double]
getNeededBalanceData (as, ps) f = [getBalance a | a <- as, p <- ps, f p, getAccountPersonId a == getPersonId p]

getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
getAverageBalance d f = sum xs / length' xs
    where xs = getNeededBalanceData d f

length' :: [Double] -> Double
length' = foldr (\ x -> (+) 1) 0

getNeededCityData :: ([Account], [Person]) -> [String] -> [Double]
getNeededCityData (as, ps) strs = [getBalance a | a <- as, p <- ps, s <- strs, s == getPersonCity p, getAccountPersonId a == getPersonId p]

averageBalanceOfCities :: ([Account], [Person]) -> [String] -> Double
averageBalanceOfCities d strs = sum xs / length' xs
    where xs = getNeededCityData d strs