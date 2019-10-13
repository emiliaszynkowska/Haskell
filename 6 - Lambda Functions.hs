--there are multiple ways of writing an add function
add :: Num a => a -> a -> a
--way 1
add x y = x + y
--way 2
add' = \x -> \y -> x + y