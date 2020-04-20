--exercise 1
--doubles a number
double :: Int -> Int
double x = x + x

--exercise 2
--returns the sum of a list
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

--exercise 3
--returns the product of a list
productList :: [Int] -> Int
productList [] = 0
productList [x] = x
productList (x:xs) = x * productList xs

--exercise 4
--quicksort in ascending order
--quicktros in descending order
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort ls ++ [x] ++ quicksort rs
                   where 
                     ls = [ a | a <- xs , a <= x ]
                     rs = [ a | a <- xs , a > x ]
quicktros :: [Int] -> [Int]
quicktros [] = []
quicktros (x:xs) = reverse (quicksort ls ++ [x] ++ quicksort rs)
                   where 
                     ls = [ a | a <- xs , a <= x ]
                     rs = [ a | a <- xs , a > x ]

--exercise 5
--quicksort which removes duplicates
quickshort :: [Int] -> [Int]
quickshort [] = []
quickshort (x:xs) = quickshort ls ++ [x] ++ quickshort rs
                   where 
                     ls = [ a | a <- xs , a < x ]
                     rs = [ a | a <- xs , a > x ]

--exercise 6
--using BIDMAS
bidmas = do
    print ((2^3)*4)     --32
    print ((2*3)+(4*5)) --26
    print (2+(3*(4^5))) --3074
    print ((2^2)+(2^2)) --8

--exercise 7
--correct this function
divideByLength = a `div` length xs
        where 
           a = 10 
           xs = [1,2,3,4,5]
    
--exercise 8
--types:
--String, Char, Bool
--Int, Integer, Float, Double
--classes: 
--Eq, Ord 
--Num, Floating, Integral, Fractional
--Enum, Bounded
--Show, Read
let a = ['a','b','c']               --[Char]
let b = ('a','b','c')               --(Char,Char,Char)
let c = ['a',3,True]                --none
let d = ('a',3,True)                --(Char,Num,Bool)
let e = [(False, '0'),(True,'1')]   --[(Bool,Char)]
let f = ([True,False],['0','1'] )   --[(Bool,Bool),(Char,Char)]
let g = [tail,init,reverse]         --[[a]->[a]]
let h = []                          --[a]
let i = 2 : 3 : [] : 4 : 5 : []     --Num [a] => [[a]]
let j = [] : []                     --[[a]]

--exercise 9
bools :: [Bool]
bools = [True,False]

nums :: [[Int]]
nums = [[1],[1,2],[1,2,3]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply x y = x y

--exercise 10
second xs = head (tail xs)      --[a] -> a
swap (x,y) = (y,x)              --(a,a) -> (a,a)
pair x y = (x,y)                --a -> a -> (a,a)
double x = x*2                  --Num a => a -> a
palindrome xs = reverse xs == xs--[a] -> Bool
