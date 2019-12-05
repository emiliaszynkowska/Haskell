{-# LANGUAGE DeriveGeneric #-}

module Exercises (histogram,approxSqrt,longestCommonSubsequence,neighbours,findBonding,insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..),Instruction(..),Stack,SMProg,evalInst,findMaxReducers,optimalPower) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import qualified Data.List as S

--exercise 1
--returns a list that counts the frequency of values drawn from xs in the ranges 0..n-1, n..2n-1, 2n..3n-1 and so on, until all values in xs have been counted
--e.g. histogram 5 [1, 2, 10, 4, 7, 12] = [3, 1, 2]
histogram :: Int -> [Int] -> [Int]
histogram n xs 
    | n > 0 = histogram3 n n xs
    | otherwise = error "Error: negative number/ zero number"

histogram2 :: Int -> [Int] -> [Int]
histogram2 m [] = []
histogram2 m xs = [length (filter (<m) xs)]

histogram3 :: Int -> Int -> [Int] -> [Int]
histogram3 n oldn [] = []
histogram3 n oldn xs = (histogram2 n xs) ++ (histogram3 (n+oldn) oldn (filter (>=n) xs))

--exercise 2
--returns the square root using the Babylonian method
--stops when (d-c < epsilon) and returns the result
approxSqrt :: Double -> Double -> Double
approxSqrt d epsilon 
    | d < 0 = error "negative number"
    | epsilon <= 0 = error "negative number / zero number"
    | otherwise = approxSqrt2 d epsilon d

approxSqrt2 :: Double -> Double -> Double -> Double
approxSqrt2 d epsilon c 
    | abs((sqrt d)-c) < epsilon = c
    | otherwise = approxSqrt2 d epsilon ((c+d/c)/2)

--exercise 3
--when given a list of lists xss, returns the longest subsequence of these lists
longestCommonSubsequence :: Eq a => [[a]] -> [a]
longestCommonSubsequence [] = []
longestCommonSubsequence [[]] = []
longestCommonSubsequence (xs:[]) = xs
longestCommonSubsequence (xs:ys:xss)
    | length xss == 0 = findCommonElements xs ys
    | otherwise = longestCommonSubsequence (findCommonElements xs ys : xss)
    
findCommonElements :: Eq a => [a] -> [a] -> [a]
findCommonElements xs [] = []
findCommonElements [] ys = []
findCommonElements (x:xs) (y:ys) 
        | x == y = x : findCommonElements xs ys
        | length list1 > length list2 = list1
        | otherwise = list2
        where list1 = findCommonElements (x:xs) ys
              list2 = findCommonElements xs (y:ys)

--exercise 4
type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double
--takes two points and returns the distance between them
metric (a,b) (c,d) = sqrt $ ((c-a)^2 + (d-b)^2) 
--neighbours k d p xs returns a list of the k nearest neighbours
neighbours :: Ord a => Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k d p xs 
    | k < 0 = error "negative k"
    | otherwise = take k (allNeighbours p d xs)
--gets all neighbours, ordered by distance
allNeighbours :: Ord a => Point a -> Metric a -> [Point a] -> [Point a]
allNeighbours p d xs = map snd (orderByDistance p d xs)
-- --sorts mapDistance
orderByDistance :: Ord a => Point a -> Metric a -> [Point a] -> [(Double,Point a)]
orderByDistance p d xs = S.sort (mapDistance p d xs)
--gives an association (distance, point)
mapDistance :: Ord a => Point a -> Metric a -> [Point a] -> [(Double,Point a)]
mapDistance p d xs = zip (map (d p) xs) xs

--exercise 5
--p = predicate, xs = input list
--function B to S is a bonding if it is total and symmetric
findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding p [] = Just []
findBonding p (x:xs) 
    | b == [] = Nothing
    | otherwise = head b
        where a = findPairs p x xs
              b = [case bonding of {Just b -> Just (pair : (snd pair, fst pair) : b); Nothing -> Nothing} | pair <- a, let bonding = findBonding p [x | x <- xs, x /= snd pair], bonding /= Nothing] 
--find all pairs for one element
findPairs :: Eq a => (a -> a -> Bool) -> a -> [a] -> [(a,a)]
findPairs p x xs = [(x,y) | y <- xs, p x y, x /= y]

--exercise 6
data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)

--takes a value and a zipper
--inserts a node with the new value 
--navigates from the current node to the new point 
--increments the counter for visited nodes
insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode v (Leaf,[]) = (Node Leaf v 1 Leaf,[])
insertFromCurrentNode v (tree,ts) = insertFromCurrentNode2 v (goRootInc(tree,ts))
--inner function
insertFromCurrentNode2 :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode2 v (Leaf,ts) = (Node Leaf v 1 Leaf,ts)
insertFromCurrentNode2 v (Node ltree x c rtree,ts)
    | compare x v == EQ = (Node ltree x c rtree,ts)
    | compare x v == GT = insertFromCurrentNode2 v (increment(goLeft(Node ltree x c rtree,ts)))
    | compare x v == LT = insertFromCurrentNode2 v (increment(goRight(Node ltree x c rtree,ts)))

--goes into the left tree, adds L to the trail
goLeft :: Ord a => Zipper a -> Zipper a
goLeft (Node ltree x c rtree, ts) = (ltree, (L x c rtree : ts))
--goes into the right tree, adds R to the trail
goRight :: Ord a => Zipper a -> Zipper a
goRight (Node ltree x c rtree, ts) = (rtree, (R x c ltree : ts))
--moves up to the parent
goUp :: Ord a => Zipper a -> Zipper a
goUp (tree,[]) = (tree,[])
goUp (ltree, (L x c rtree) : ts) = (Node ltree x c rtree, ts)
goUp (rtree, (R x c ltree) : ts) = (Node ltree x c rtree, ts)
--moves to the root
goRoot :: Ord a => Zipper a -> Zipper a   
goRoot (tree, []) = (tree, []) 
goRoot tree = goRoot (goUp tree)
--moves to the root and increments
goRootInc :: Ord a => Zipper a -> Zipper a   
goRootInc (tree, []) = (tree, []) 
goRootInc tree = goRoot (increment(goUp tree))
--increments the counter
increment :: Ord a => Zipper a -> Zipper a
increment (Leaf,ts) = (Leaf,ts)
increment (Node ltree x c rtree,ts) = (Node ltree x (c+1) rtree,ts)

--takes a list of values and inserts them into the empty zipper in list order
mkTree :: Ord a => [a] -> Zipper a
mkTree = foldl (\y -> \x -> insertFromCurrentNode x y) (Leaf,[])

--exercise 7 
--add pops the top two elements of the stack, adds them and pushes the result back on to the stack
--mul pops the top two elements of the stack, multiplies them and pushes the result back on to the stack
--dup pushes another copy of the top element on to the stack
--pop removes the top element of the stack
data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

--takes a stack and an SM program and returns the resulting stack
evalInst :: Stack -> SMProg -> Stack
evalInst [] prog = error "error"
evalInst stack [] = stack
evalInst [x] (Add:prog) = error "error"
evalInst [x] (Mul:prog) = error "error"
evalInst (x:y:stack) (Add:prog) = evalInst ((x+y):stack) prog
evalInst (x:y:stack) (Mul:prog) = evalInst ((x*y):stack) prog
evalInst (x:stack) (Dup:prog) = evalInst (x:x:stack) prog
evalInst (x:stack) (Pop:prog) = evalInst (stack) prog

--exercise 8
--a maximal reducer returns the highest possible value for that input stack
--returns all maximal reducers for the given input stack
findMaxReducers :: Stack -> [SMProg]
findMaxReducers [] = []
findMaxReducers stack = [prog | prog <- instCombos, head (evalInst stack prog) == maxResult stack instCombos]
  where instCombos = allInstCombos ((length stack)-1)

-- Find maximum result sequence of instructions combination can produce on a stack
maxResult :: Stack -> [SMProg] -> Int
maxResult stack progs = maximum [head (evalInst stack prog) | prog <- progs]
 
-- Return all combinations thereof for a given sequence of instructions
allInstCombos :: Int -> [SMProg]
allInstCombos 0 = [[]]
allInstCombos n = map ([Add]++) (allInstCombos (n-1)) ++ map ([Mul]++) (allInstCombos (n-1)) ++ map ([Pop]++) (allInstCombos (n-1))

--exercise 9
--given an input value of a positive integer n
--returns a SM program that will transform a stack [x] to the stack [x^n] 
optimalPower :: Int -> SMProg
optimalPower (-1) = error "negative"
optimalPower 0 = error "zero"
optimalPower n 
    | n == 1 = []
    | n == 15 = [Dup,Dup,Mul,Dup,Mul,Mul,Dup,Dup,Mul,Mul]
    | odd n = [Dup] ++ optimalPower (n-1) ++ [Mul]
    | even n = [Dup, Mul] ++ optimalPower (n `div` 2)
