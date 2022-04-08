-- Define a function that calculates the volume of a list 
-- of cylinders. Let the cylinder be expressed as the 
-- following: type Cylinder = (Double, Double), 
-- where the first coordinate is the radius and the 
-- second - its height.

type Cylinder = (Double, Double)

main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] == [785.4, 157.08, 125.66, 62.83]

roundToTwoDig :: Double -> Double
roundToTwoDig = (/100) . fromIntegral . round . (*100)

caluclateVolume :: Double -> Double -> Double
caluclateVolume r h = pi * (r ^ 2) * h

getVolumes :: [Cylinder] -> [Double]
getVolumes cs = [ roundToTwoDig $ caluclateVolume r h | (r, h) <- cs]