--Class definition of Monad
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    return = pure

--Monad and lists
 instance Monad [] where
    ->>= :: [a] -> (a -> [b]) -> [b]
    xs >>= f = [ y | x <- xs, y <- f x ]