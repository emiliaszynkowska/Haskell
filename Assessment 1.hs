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

--exercise A2
--returns the square root using the Babylonian method
--stops when (d-c < epsilon) and returns the result
approxSqrt :: Double -> Double -> Double
approxSqrt 0 epsilon = 0
approxSqrt d epsilon = approxSqrt2 d epsilon d
approxSqrt2 :: Double -> Double -> Double -> Double
approxSqrt2 d epsilon c 
    | abs((sqrt d)-c) < epsilon = c
    | otherwise = approxSqrt2 d epsilon ((c+d/c)/2)

--exercise A3
--when given a list of lists xss, returns the longest subsequence of these lists
longestCommonSubsequence :: Ord a => [[a]] -> [a]
longestCommonSubsequence xss 
    | length xss > 2 = longestCommonSubsequence (findCommonElements(take 2 xss) : (drop 2 xss))
    | length xss == 2 = removeDuplicates (findCommonElements xss)
    | length xss == 1 = head xss
    | otherwise = []
findCommonElements :: Ord a => [[a]] -> [a]
findCommonElements [xs,ys] = [x | x <- xs, x <- ys, x `elem` xs, x `elem` ys]
removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
