factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]

prime :: Int -> Bool
prime n = factors n == [1,n]

primesUpTo :: Int -> [Int]
primesUpTo n = [ x | x <- [0..n], prime x ]

--takes two lists a and b and produces one list of (a,b) pairs
zipList :: [a] -> [b] -> [(a,b)]
zipList [] _ = []
zipList _ [] = []
zipList (x:xs) (y:ys) = (x,y) : zip xs ys

--returns pairs of adjacent elements in a list
pairs :: [a] -> [(a,a)] 
pairs xs = zipList xs (tail xs) 

--checks if a list is sorted or not
sorted :: Ord a => [a] -> Bool 
sorted xs = and [ x <= y | (x,y) <- pairs xs ]

--finds all the positions of a certain element in a list
positions :: Eq a => a -> [a] -> [Int] 
positions x xs = [ p | (y,p) <- zipList xs [0..], x == y ]

--finds the number of occurences of a character in a string
count :: Char -> String -> Int 
count x xs = length [ y | y <- xs , x == y ]