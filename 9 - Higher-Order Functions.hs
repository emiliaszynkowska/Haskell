--doubles each item in a list
doubleList :: [Int] -> [Int] 
doubleList [] = [] 
doubleList (x:xs) = 2*x : doubleList xs

--doubleList using the map function
easyDoubleList :: [Int] -> [Int]
easyDoubleList xs = map (2*) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] = v
foldl f v (x:xs) = foldl f (f v x) xs