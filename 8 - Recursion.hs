--returns n!
fac 0 = 1
fac n = n * fac (n-1)

--product of a list
product :: Num => [a] -> a 
product [] = 1 
product (x:xs) = x * product xs

--length of a list
length :: [a] -> Int 
length [] = 0 
length (x:xs) = 1 + length xs