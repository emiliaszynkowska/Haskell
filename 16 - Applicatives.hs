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

--e.g. pure (4 +) <*> [1,2,3] 
--= [5,6,7]