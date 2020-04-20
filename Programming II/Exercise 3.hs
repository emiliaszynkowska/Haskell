module Exercises (longestCommonSubsequence) where

--exercise 3
--when given a list of lists xss, returns the longest subsequence of these lists
longestCommonSubsequence :: Eq a => [[a]] -> [a]
longestCommonSubsequence [] = []
longestCommonSubsequence [[]] = []
longestCommonSubsequence (xs:[]) = xs
longestCommonSubsequence (xs:ys:xss)
    | length xss == 0 = findCommonElements xs ys
    | otherwise = longestCommonSubsequence (findCommonElements xs ys : xss)
    
findCommonElements :: Eq a => [a] -> [a] -> [a]
findCommonElements xs [] = []
findCommonElements [] ys = []
findCommonElements (x:xs) (y:ys) 
        | x == y = x : findCommonElements xs ys
        | length list1 > length list2 = list1
        | otherwise = list2
        where list1 = findCommonElements (x:xs) ys
              list2 = findCommonElements xs (y:ys)
