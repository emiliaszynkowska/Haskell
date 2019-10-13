--adds numbers
add :: (Int,Int) -> Int
add(x,y) = x+y
add' :: Int -> (Int -> Int)
add' x y = x+y

--multiplies numbers
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z

--returns a list of all integers up to x
zeroTo :: Int -> [Int]
zeroTo x = [0..x]