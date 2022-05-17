-- Define a function that accepts a list of countries and 
-- returns the name of the country with the lowest average 
-- yearly temperature (the coldest country).

-- Use the following types:

-- type Name = String
-- type Capital = Name
-- type AvgYearlyTemperature = Double
-- type Elevation = Int

-- data City = City Name Elevation AvgYearlyTemperature
-- data Country = Country Name Capital [City]
-- Implemenation detail: Solve the task using folding!

main :: IO()
main = do
    print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

countryCities :: Country -> [City]
countryCities (Country _ _ cs) = cs

countryName :: Country -> Name
countryName (Country n _ _) = n

cityTemp :: City -> Double
cityTemp (City _ _ t) = t

coldestCity :: [City] -> City
coldestCity = foldr1 (\c1@(City _ _ t1) c2@(City _ _ t2) -> if t1 < t2 then c1 else c2)

coldestCapital :: [Country] -> Name
coldestCapital = countryName . foldr1 (\c1 c2 -> if lowestTemp c1 < lowestTemp c2 then c1 else c2)
    where lowestTemp c = cityTemp $ coldestCity $ countryCities c