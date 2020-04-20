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

tupp1 :: (a,a) -> (a,a) -> a
tupp1 (a,b) (c,d) = d
tupp2 :: a -> a -> a -> a -> a
tupp2 a b c d = a

--exercise 4
--list generating pattern
--p = predicate
--h = adding function
--t = remainder function
--x = element
unfold p h t x 
    | p x = []
    | otherwise = h x : unfold p h t (t x)
int2bin x = reverse (unfold (==0) (`mod` 2) (`div` 2) x)

--exercise 5
--takes two function and alternately applies them 
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs
  
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

--exercise 8
data Nat = Zero | Succ Nat deriving (Eq,Ord,Show,Read)
--sees if a natural number is even 
evenn :: Nat -> Bool
evenn (Zero) = True
evenn (Succ x) = not (evenn x)
--sees if a natural number is odd
oddd :: Nat -> Bool
oddd (Zero) = False
oddd (Succ x) = not (oddd x)
--adds natural numbers
addd :: Nat -> Nat -> Nat
addd Zero Zero = Zero
addd Zero x = x
addd x Zero = x
addd (Succ x) (Succ y) = addd x (Succ(Succ y))
--multiplies natural numbers
multt :: Nat -> Nat -> Nat
multt Zero Zero = Zero
multt Zero x = Zero
multt x Zero = Zero
multt (Succ x) y = addd y (multt x y)