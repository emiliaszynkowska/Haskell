--creating trees
data Tree a = Leaf a | Node (Tree a) a (Tree a)
data ITree a = Leaf | Node a (ITree a) (ITree a)
data LTree a = Leaf a | Node (LTree a) (LTree a)
data MTree a = Node a [MTree a]

--tree mapping
TreeMap :: (a -> b) -> LTree a -> LTree b
TreeMap f (Leaf x) = Leaf (f x)
TreeMap f (Node l r) = Node (TreeMap f l) (TreeMap f r)

--tree navigation
data Direction = L | R
type Directions = [Direction]

elemAt :: Directions -> ITree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

--trail navigation
type Trail = [Direction]
goLeft :: (Tree a, Trail) -> (Tree a, Trail)
goLeft (Node _ l _, ts) = (l, L:ts)
goRight :: (Tree a, Trail) -> (Tree a, Trail)
goRight (Node _ _ r, ts) = (r, R:ts)

myTree = Node 5 (Node 4 (Node 2) Node 3)
myTree -: goLeft 

--zippers
data Direction a = L a (Tree a) | R a (Tree a)
type Zipper a = (Tree a, Trail a)

modify :: (a → a) → Zipper a → Zipper a   
modify f (Node x l r, ts) = (Node (f x) l r, ts)   
modify f (Leaf, ts) = (Leaf, ts)  

attach :: Tree a → Zipper a → Zipper a   
attach t ( _ , ts) = (t, ts)  

tgoRoot :: Zipper a → Zipper a   
goRoot (t , []) = (t, []) 
goRoot z = goRoot (goUp z)

--red-black tree
type Colour = String
--way 1
data RBTree a = Leaf a Colour | Node a Colour (RBTree a) (RBTree a)
--way 2
data RBTree a = Leaf a | RedNode a (RBTree a) (RBTree a) | BlackNode a (RBTree a) (RBTree a)

--abstract syntax tree
data Expr = Val Int | Add Expr Expr | Sub Expr Expr
 
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y

--propositional logic
data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop

eval :: Subst -> Prop -> Bool
eval s (Const n) = n
eval s (Var v) = find v s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q