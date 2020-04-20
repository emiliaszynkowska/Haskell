--H-99: Ninety-Nine Haskell Problems
--https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

--1. find the last element of a list
lastElement :: [Int] -> Int
lastElement xs = xs !! (length xs - 1)

--2. find the second last element of a list
secondLastElement :: [Int] -> Int
secondLastElement xs
    | length xs == 0 = length xs
    | length xs == 1 = head xs
    | otherwise =  xs !! (length xs - 2)

--3. find the kth element of a list
elementAt :: [Int] -> Int -> Int
elementAt xs k = xs !! k

--4. find the length of a list
listLength :: [Int] -> Int
listLength xs = length xs

--5. reverse a list
reverseList :: [Int] -> [Int]
reverseList xs = reverse xs

--6. find whether the list is a palindrome
isPalindrome :: [Int] -> Bool
isPalindrome xs = xs == (reverse xs) 

--7. flatten nested lists
flattenList :: [[a]] -> [a]
flattenList xss = [ x | xs <- xss, x <- xs ]




