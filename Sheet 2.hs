---exercise 1
--there are multiple ways to implement the 'last' function:
--way 1
last1 :: [a] -> a
last1 [] = error "empty list"
last1 (x:[]) = x
last1 (x:xs) = last xs
--way 2
last2 :: [a] -> a
last2 xs = xs !! (length xs - 1)
--way 3
last3 :: [a] -> a
last3 [x] = x
last3 xs = last3 (tail xs)

--exercise 2
--returns the third element in a list
third :: [a] -> a
third xs 
    | length xs >= 3 = xs !! 2
    | otherwise = error "invalid list"

--exercise 3
--there are multiple ways to implement the 'safetail' function:
--way 1
safetail1 :: [a] -> [a]
safetail1 [] = []
safetail1 [x] = []
safetail1 xs = tail xs
--way 2
safetail2 :: [a] -> [a]
safetail2 xs
    | length xs == 0 = []
    | length xs == 1 = []
    | otherwise = tail xs
--way 3
safetail3 :: [a] -> [a]
safetail3 xs =
    if length xs == 0
        then []
        else if length xs == 1
            then []
            else tail xs

--exercise 4
halve :: [a] -> ([a],[a])
halve xs =
    if length xs == 0
        then ([],[])
        else if (length xs `mod` 2) /= 0
            then error "not an even list"
            else (one,two)
                where one = take (length xs `div` 2) xs
                      two = drop (length xs `div` 2) xs

--exercise 5




