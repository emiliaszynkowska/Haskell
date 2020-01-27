class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

--Applicative and Maybe
instance Applicative Maybe where
    pure = Just
    (<*>) Nothing _ = Nothing
    (<*>) _ Nothing = Nothing
    (<*>) (Just f) (Just x) = Just (f x)

--Applicative and lists
instance Applicative [] where
  pure a = [a]
  fs <*> xs = [f x | f <- fs, x <- xs]

example1 = (+) <$> (Just 1) <*> (Just 2) 
example2 = [(+3),(+100),(*3)] <*> [3,4,5]
example3 = [(+),(^)] <*> [1,2] <*> [3,4]

