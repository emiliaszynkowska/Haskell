import Data.List 
import Data.Graph
--exercise 1
--LT = less than, EQ = equal, GT = greater than
--occurs determines whether a tree contains the given element
data Tree1 a = Leaf1 a | Node1 (Tree1 a) a (Tree1 a)
occurs1 :: Ord a => a -> Tree1 a -> Bool
occurs1 x (Leaf1 a) 
    | x == a = True
    | otherwise = False
occurs1 x (Node1 ltree a rtree) 
    | x == a = True
    | x < a = occurs1 x ltree
    | x > a = occurs1 x rtree

--exercise 2
flatten :: Ord a => Tree1 a -> [a]
flatten tree = sort (makeList tree)
makeList :: Ord a => Tree1 a -> [a]
makeList (Leaf1 a) = a : []
makeList (Node1 ltree a rtree) = (makeList ltree) ++ a:[] ++ (makeList rtree)

--exercise 3
data Expr = Val Int | Add Expr Expr | Sub Expr Expr
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
size :: Expr -> Int
size (Val n) = 1
size (Add x y) = (size x) + (size y)
size (Sub x y) = (size x) + (size y)

--exercise 4
data Prop = Const Bool | Var Char | Not Prop 
            | And Prop Prop | Imply Prop Prop
            deriving Show
data Form = Negative | Positive | Neither deriving (Eq,Show)
--negative form = all occurences are negative
--positive form = all occurences are positive
--mixed form    = neither of the above
getForm :: Prop -> Form
getForm (Var x) = Positive
getForm (Not(Var x)) = Negative
getForm (Not x) 
    | getForm x == Positive = Negative
    | getForm x == Negative = Positive
    | otherwise = Neither
getForm (Imply x y) 
    | getForm x == Positive = Negative
    | getForm x == Negative = Positive
    | otherwise = Neither

--exercise 6
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving Show
data Direction a = L (Tree2 a) | R (Tree2 a) deriving Show
type Trail a = [Direction a]
type Zipper a = (Tree2 a, Trail a)
--goes into the left tree, adds L to the trail
goLeft :: Zipper a -> Zipper a
goLeft (Node2 ltree rtree, ts) = (ltree, (L rtree : ts))
--goes into the right tree, adds R to the trail
goRight :: Zipper a -> Zipper a
goRight (Node2 ltree rtree, ts) = (rtree, (R ltree : ts))
--moves up to the parent
goUp :: Zipper a -> Zipper a
goUp (tree, L rtree : ts) = (Node2 tree rtree, ts)
goUp (tree, R ltree : ts) = (Node2 ltree tree, ts)
--moves to the root
goRoot :: Zipper a -> Zipper a   
goRoot (tree, []) = (tree, []) 
goRoot x = goRoot (goUp x)

--increments the 2nd leftmost and 2nd rightmost leaves by 1
inc2LR :: Tree2 Int -> Tree2 Int
inc2LR (Leaf2 a) = Leaf2 a
inc2LR tree = fst(travel2(travel1(tree, [])))
travel1 :: Zipper Int -> Zipper Int
travel1 z = goRoot(incrementLeaf(findSecondLeft(goFullyLeft z)))
travel2 :: Zipper Int -> Zipper Int
travel2 z = goRoot(incrementLeaf(findSecondRight(goFullyRight z)))

--finds the leftmost leaf
goFullyLeft :: Zipper a -> Zipper a
goFullyLeft (Leaf2 a, ts) = (Leaf2 a, ts)
goFullyLeft (Node2 ltree rtree, ts) = goFullyLeft (goLeft (Node2 ltree rtree, ts))
--finds the rightmost leaf
goFullyRight :: Zipper a -> Zipper a
goFullyRight (Leaf2 a, ts) = (Leaf2 a, ts)
goFullyRight (Node2 ltree rtree, ts) = goFullyRight (goRight (Node2 ltree rtree, ts))
--finds the second leftmost leaf
findSecondLeft :: Zipper a -> Zipper a
findSecondLeft (Leaf2 a, ts) = goRight(goUp (Leaf2 a, ts))
--finds the second rightmost leaf
findSecondRight :: Zipper a -> Zipper a
findSecondRight (Leaf2 a, ts) = goLeft(goUp (Leaf2 a, ts))
--increments a leaf's value by 1
incrementLeaf :: Zipper Int -> Zipper Int
incrementLeaf (Leaf2 a, ts) = (Leaf2 (a+1), ts)

--exercise 7
--creates a graph with 1000 nodes
--there is an edge from every even-numbered node N to node N+1
--there is an edge from every odd-numbered node N to node (N div 5)

--type Vertex = Int
--type Edge = (Vertex, Vertex)
--type Graph = Array Vertex [Vertex]

--makeNodeGraph makes a graph of nodes, calls the built-in function buildG
--buildG takes the parameters buildG (lowerlimit, upperlimit) (listofedges)
makeNodeGraph :: Graph
makeNodeGraph = buildG (0,1000) (listOfEdges 0)
listOfEdges :: Int -> [Edge] 
listOfEdges n 
    | n == 1000 = []
    | even n = (n,n+1) : listOfEdges (n+1)
    | odd n = (n,n `div` 5) : listOfEdges (n+1)
--determines if an edge exists 
isReachable :: Int -> Int -> Bool
isReachable x y 
    | x == 0 && y /= 1 = False
    | elem (x,y) (edges makeNodeGraph) = True
    | elem (x,x+1) (edges makeNodeGraph) = isReachable (x+1) y
    | elem (x, x `div` 5) (edges makeNodeGraph) = isReachable (x `div` 5) y
    | otherwise = False


    