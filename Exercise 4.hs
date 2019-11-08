module Exercises (neighbours) where
import qualified Data.List as S

--exercise 4
type Point a = (a,a)
type Metric a = Point a -> Point a -> Double
--takes two points and returns the distance between them
metric (a,b) (c,d) = sqrt $ ((c-a)^2 + (d-b)^2) 
--neighbours k d p xs returns a list of the k nearest neighbours
--neighbours :: Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k d p xs 
    | k < 0 = error "negative k"
    | otherwise = take k (allNeighbours p d xs)
--gets all neighbours, ordered by distance
allNeighbours p d xs = map snd (orderByDistance p d xs)
-- --sorts mapDistance
orderByDistance p d xs = S.sort (mapDistance p d xs)
--gives an association (distance, point)
mapDistance p d xs = zip (map (d p) xs) xs