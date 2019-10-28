--exercise 1
--decides if all elements of a list satisfy a predicate
alll :: (a -> Bool) -> [a] -> Bool
alll p [] = True
alll p (x:xs) 
    | p x == False = False
    | otherwise = alll p xs
--decides if at least one elements of list satisfies a predicate
anyy :: (a -> Bool) -> [a] -> Bool
anyy p [] = False
anyy p (x:xs)
    | p x == True = True
    | otherwise = anyy p xs
--takes elements until the predicate is satisfied
takeWhilee :: (a -> Bool) -> [a] -> [a]
takeWhilee p [] = []
takeWhilee p (x:xs) 
    | p x == False = []
    | p x == True = x : takeWhile p xs
--drops elements until a predicate is satisfied
dropWhilee :: (a -> Bool) -> [a] -> [a]
dropWhilee p [] = []
dropWhilee p (x:xs)
    | p x == False = dropWhilee p xs
    | p x == True = x : xs

--exercise 2
--takes a list of digits which represent a decimal number
--returns an integer which represents the number
--e.g. dec2Int [1,2,3,4] = 1234
dec2Int :: [Int] -> Int
dec2Int xs = foldl addDigit 0 xs
addDigit n d = 10*n + d

--exercise 3
--converts an uncurried function to a curried function
curryy :: ((a,a) -> (a,a) -> a) -> (a -> a -> a -> a -> a) 
curryy function a b c d = function (a,b) (c,d)
--converts a curried function to an uncurried function
uncurryy :: (a -> a -> a -> a -> a) -> ((a,a) -> (a,a) -> a)
uncurryy function (a,b) (c,d) = function a b c d

tupp :: a -> a -> a -> a -> a
tupp a b c d = a

--exercise 5
--takes two function and alternately applies them 
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f(x) : altaltMap f g xs
altaltMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altaltMap f g [] = []
altaltMap f g (x:xs) = g(x) : altMap f g xs
  
--exercise 7
--constructs a balanced binary tree from the given list
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show
toTree :: Ord a => [a] -> Tree a
toTree (x:xs) = foldr insert (Node Leaf x Leaf) (reverse xs)

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node tree1 value tree2)
    | x == value = Node tree1 value tree2
    | x < value = Node (insert x tree1) value tree2
    | x > value = Node tree1 value (insert x tree2)