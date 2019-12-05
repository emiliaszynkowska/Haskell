--Class definition of functor
class Functor f where
    fmap :: (a -> b) -> f a -> f b

--Relation of functor to fmap
instance Functor [] where
    fmap = map

--Functor and maybe
data Maybe a = Nothing | Just a

instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

--Functor and Tree
data Tree a = Empty | Node a (Tree a) (Tree a)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x ltree rtree) = Node (f x) (fmap f ltree) (fmap f rtree)

