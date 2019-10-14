--exercise A1
--returns a list that counts the frequency of values drawn from xs in the ranges 0..n-1, n..2n-1, 2n..3n-1 and so on, until all values in xs have been counted
--e.g. histogram 5 [1, 2, 10, 4, 7, 12] = [3, 1, 2]
histogram :: Int -> [Int] -> [Int]
histogram (x:xs) = 
    let ns = [m | m <- [0..(ns !! (length xs - 1))], (m+1) `mod` n == 0]
             
check :: Int -> [Int] -> [Int]
check max (x:xs) = [y | y <- [0..max]]

--exercise A2
--approxSqrt :: Double -> Double -> Double