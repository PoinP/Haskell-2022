-- Define a function that accepts a list of countries '
-- and returns the name of the country with the highest 
-- capital, i.e. the capital on the highest elevation.

-- Implementation detail: Solve the task using folding.

main :: IO()
main = do
    print $ highestCapital db == "Bulgaria"

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]
type DataBase = [Country]

db :: DataBase
db = 
    [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]),
    (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), 
    (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])]

highestCapital :: DataBase -> Name
highestCapital [] = error "ERROR: Empty database!"
highestCapital cs = fst $ foldl (\ acc (Country n capital cs) -> let elevation = capitalElevation capital cs in if elevation > snd acc then (n, elevation) else acc) ("", 0) cs
    where
        capitalElevation capital cs = head [e | (City n e _) <- cs, n == capital]