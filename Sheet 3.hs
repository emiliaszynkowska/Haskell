import qualified Data.List as S

--exercise 1
--calculates the sum of squares of odd numbers, and cubes of even numbers, for the first 100 integers
sum100Integers :: Int
sum100Integers = sum [x^2 | x <- [1..100], odd x] + sum [y^3 | y <- [1..100], even y]

--exercise 2
--returns a grid of a given size
grid :: Int -> Int -> [(Int,Int)]
grid a b = [(x,y) | x <- [0..a], y <- [0..b]]
square :: Int -> [(Int,Int)]
square n = [(x,y) | x <- [0..n], y <- [0..n], x /= y]

--exercise 3
--makes a list of n copies of the given input
rreplicate :: Int -> a -> [a]
rreplicate x y
    | x > 0  = y : replicate (x-1) y
    | otherwise = []

--exercise 4
--returns pythagorean triples up to a given limit n
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | z <- [2..n], y <- [2..n], x <- [2..n], x^2 + y^2 == z^2]

--exercise 5 
--returns perfect numbers up to a given limit n
--perfect number: one which equals the sum of all its factors, excluding itself
perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], ((sum (factors x)) - x) == x]
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

--exercise 6
--implement the positions functions using the find function
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (mapit xs)
mapit :: [a] -> [(a,Int)]
mapit xs = zip [x | x <- xs] [y | y <- [0..(length xs - 1)]]
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [ v | (k',v) <- t, k==k']

--exercise 7
--calculates the scalar product: sum of x1*y1, x2*y2 .. xn*yn
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct [] [] = 0
scalarProduct (x:xs) (y:ys) 
    | length xs == length ys = x*y + scalarProduct xs ys
    | otherwise = error "uneven lists"

--exercise 8 
--implements euclid's algorithm for calculating the largest common factor of two numbers
--if the two numbers are equal this number is the result
--otherwise, subtract the smaller number from the larger and repeat
euclid :: Int -> Int -> Int
euclid x y
    | x == y = x
    | x > y = euclid (x-y) y
    | y > x = euclid x (y-x)

--exercise 9 
--merges 2 sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = S.sort(xs ++ ys)
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSort h1 `merge` mergeSort h2
    where h1 = take (length xs `div` 2) xs
          h2 = drop (length xs `div` 2) xs

