--exercise 1
append :: [a] -> [a] -> [a]
append xs [] = xs
append [] ys = ys
append (x:xs) ys = x : (xs ++ ys)

--show that xs ++ [] = xs 
--and xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

-- 1.
-- base case: xs = []
-- then xs ++ [] = [] ++ [] = []
-- as append xs [] = []

-- inductive case: xs ++ [] = xs
-- if xs = (x:xs)
-- then xs ++ [] 
-- = (x:xs) ++ [] 
-- = x : (xs ++ [])  by definition
-- = x : xs          by the inductive hypothesis

-- 2.
-- base case: xs = []
-- then xs ++ (ys ++ zs) 
-- = [] ++ (ys ++ zs)
-- = ([] ++ ys) ++ zs
-- = ys ++ zs

-- inductive case: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
-- if xs = (x:xs)
-- then xs ++ (ys ++ zs)
-- = (x:xs) ++ (ys ++ zs)
-- = x : (xs ++ (ys ++ zs))  by definition
-- = (x : (xs ++ ys)) ++ zs  by the inductive hypothesis
-- = ((x:xs) ++ ys) ++ zs    by definition
-- = (xs ++ ys) ++ zs 


--exercise 2
data Nat = Zero | Succ Nat deriving Show
--replicates x Nat times
replicatee :: Nat -> a -> [a]
replicatee Zero _ = []
replicatee (Succ n) x = x : replicatee n x
--test a predicate p for all elements of a list
alll :: (a -> Bool) -> [a] -> Bool
alll p [] = True
alll p (x:xs) = p x && all p xs

--prove that all (== x) (replicate n x) = True, for any x and any n

-- base case: replicatee Zero 0
-- replicatee Zero _ = []       
-- => replicatee Zero 0 = [] 
-- alll p [] = True             
-- => alll (==0) [] = True

-- inductive case: replicatee (Succ (Zero)) 0
-- replicatee (Succ (Zero)) 0 
-- = 0 : replicatee (Succ (Zero)) 0          by definition
-- = replicatee Zero 0 = []
-- => gives 0 : [] = [0]                     by application
-- alll p (x:xs) = p x && alll p xs          by definition
-- => alll (==0) (x:[0])                     by application
-- = ((==0) 0) && alll p []
-- = True && alll p []
-- = True && True                            by inductive hypothesis


--exercise 3
takee :: Nat -> [a] -> [a]
takee Zero _  = []                        
takee _ [] = []                           
takee (Succ n) (x:xs) = x : takee n xs
dropp :: Nat -> [a] -> [a]
dropp Zero xs = xs
dropp _ [] = []
dropp (Succ n) (_:xs) = dropp n xs

 --prove that (take n xs) ++ (drop n xs) = xs

 -- base case: xs = []
 -- take (Succ (Zero)) xs 
 -- = take (Succ (Zero)) []
 -- = []              by definition
 -- drop (Succ (Zero)) xs
 -- = drop (Succ (Zero)) []
 -- = []              by definition

 -- inductive case: xs = [1,2,3]
 -- take (Succ (Zero)) xs
 -- = take (Succ (Zero)) [1,2,3]  by definition
 -- = take (Succ (Zero)) (x:[1,2,3])
 -- = x : take (Zero) xs
 -- = 1 : take (Zero) xs          by application
 -- = 1 : []                      by definition
 -- = [1]
 -- drop (Succ (Zero)) xs
 -- = drop (Succ (Zero)) [1,2,3]  by definition
 -- = drop (Succ (Zero)) (x:[1,2,3])
 -- = drop (Succ (Zero)) (1:[2,3])
 -- = drop (Zero) [2,3]           by application
 -- = [2,3]                       by definition

 -- [1] ++ [2,3] = [1,2,3] = xs


 --exercise 4
evenn :: Nat -> Bool
evenn (Zero) = True
evenn (Succ x) = not (evenn x)

doublee :: Nat -> Nat
doublee Zero = Zero
doublee (Succ x) = Succ (Succ (doublee x))

 --prove that even (double n) = True

-- base case: n = Zero
-- double Zero = Zero by definition
-- even Zero = True   by definition

-- inductive case 1: n = (Succ n) && even (double n) = True 
-- even (double (Succ n))
-- = even (Succ (Succ(double n)))  by definition
-- = not even (Succ (double n))
-- = not not even (double n)
-- = not not True
-- = True

--exercise 5
data Tree a = Leaf a | Node (Tree a) (Tree a)

countLeaves :: Tree a -> Int
countLeaves (Leaf a) = 1
countLeaves (Node ltree rtree) = (countLeaves ltree) + (countLeaves rtree)

countNodes :: Tree a -> Int
countNodes (Leaf a) = 0
countNodes (Node ltree rtree) = 1 + (countNodes ltree) + (countNodes rtree)

--prove that in any such tree the number of leaves is always one greater than the number of nodes

-- base case: tree = Leaf a
-- countLeaves (Leaf a) = 1  by definition
-- countNodes (Leaf a) = 0   by definition

-- inductive case: tree = Node ltree rtree
-- countLeaves (Node ltree rtree) = (countLeaves ltree) + (countLeaves rtree)
-- countNodes (Node ltree rtree) = 1 + (countNodes ltree) + (countNodes rtree)

--exercise 6

--Functor Law 1: map id = id 
--Functor Law 2: map (g.h) = map g . map h

--verify these functor laws for:
instance Functor Maybe where
fmap :: (a -> b) -> Maybe a -> Maybe b
fmap g Nothing = Nothing
fmap g (Just x) = Just (g x) 

instance Functor Tree where
fmap :: (a -> b) -> Tree a -> Tree b
fmap g (Leaf x) = Leaf (g x)
fmap g (Node l r) = Node (fmap g l) (fmap g r) 

-- 1. Functor Law 1 for Maybe

-- base case: x = Nothing
-- fmap id Nothing = Nothing

-- inductive case: x
-- fmap id (Just x) 
-- = Just (id x) 
-- = (Just x)

-- 2. Functor Law 2 for Maybe 

-- base case: x = Nothing
-- fmap (g.h) Nothing = Nothing

-- inductive case: x = (Just x)
-- fmap (f.g) (Just x)
-- = Just ((f.g) x)
-- = Just (f(g x))
-- = fmap f(Just g x)
-- = fmap f(fmap g (Just x))
-- = (fmap f . fmap g) (Just x)

-- 3. Functor Law 1 for Tree

-- base case: x = (Leaf a)
-- fmap id (Leaf x) 
-- = (Leaf id x) 
-- = id (Leaf x) 
-- = Leaf x

-- inductive case: x = (Node ltree rtree)
-- fmap id (Node ltree rtree)
-- = (Node (fmap id ltree) (fmap id rtree))
-- = (Node ltree rtree)

-- 4. Functor Law 2 for Tree

-- base case: x = (Leaf a)
-- fmap (f.g) (Leaf x) 
-- = (Leaf (fmap f (fmap g x)))
-- = (Leaf (fmap f . fmap g) (x))

-- inductive case: x = (Node ltree rtree)
-- fmap (f.g) (Node ltree rtree)
-- = (Node (fmap (f.g) ltree) (fmap (f.g) rtree))
-- = (Node ((fmap f . fmap g) ltree) ((fmap f . fmap g) rtree))
-- = (Node (fmap f (fmap g ltree)) (fmap f (fmap g rtree)))
-- = fmap f (Node (fmap g ltree) (fmap g rtree))
-- = fmap f (fmap g (Node ltree rtree))
-- = fmap (f.g) (Node ltree rtree)
