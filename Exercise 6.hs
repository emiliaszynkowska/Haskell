{-# LANGUAGE DeriveGeneric #-}
module Exercises (insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..)) where
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq 

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