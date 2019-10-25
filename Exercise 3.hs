module Exercises (longestCommonSubsequence) where

--exercise A3
--when given a list of lists xss, returns the longest subsequence of these lists
longestCommonSubsequence :: Eq a => [[a]] -> [a]
longestCommonSubsequence xss 
    | length xss > 2 = longestCommonSubsequence (findCommonElements(take 2 xss) : (drop 2 xss))
    | length xss == 2 = removeDuplicates (findCommonElements xss)
    | length xss == 1 = head xss
    | otherwise = []
findCommonElements :: Eq a => [[a]] -> [a]
findCommonElements [xs,ys] = [x | x <- xs, x <- ys, x `elem` xs, x `elem` ys]
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
