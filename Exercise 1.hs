module Exercises (histogram) where

--exercise A1
--returns a list that counts the frequency of values drawn from xs in the ranges 0..n-1, n..2n-1, 2n..3n-1 and so on, until all values in xs have been counted
--e.g. histogram 5 [1, 2, 10, 4, 7, 12] = [3, 1, 2]
histogram :: Int -> [Int] -> [Int]
histogram n xs = histogram3 n n xs
histogram2 :: Int -> [Int] -> [Int]
histogram2 m [] = []
histogram2 m xs = [length (filter (<m) xs)]
histogram3 :: Int -> Int -> [Int] -> [Int]
histogram3 n oldn [] = []
histogram3 n oldn xs = (histogram2 n xs) ++ (histogram3 (n+oldn) oldn (filter (>=n) xs))
