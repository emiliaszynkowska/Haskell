import qualified Data.List as S
--module Exercises (neighbours) where

--exercise 4
type Point a = (a,a)
type Metric a = Point a -> Point a -> Double
--takes two points and returns the distance between them
metric (a,b) (c,d) = sqrt $ ((c-a)^2 + (d-b)^2) 
--neighbours k d p xs returns a list of the k nearest neighbours
-- neighbours :: Int -> Metric a -> Point a -> [Point a] -> [Point a]
-- neighbours k d p xs = 

--gets all neighbours, ordered by distance
allNeighbours p xs (r:rs) = fst r : allNeighbours p xs rs
    where rs = orderByDistance p xs

--sorts mapDistance
orderByDistance p xs = S.sort (mapDistance p xs)

--gives an association (distance, point)
mapDistance p [] = []
mapDistance p (x:xs) = (y,x) : mapDistance p xs
    where y = metric x p
