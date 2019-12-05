module Exercises (findBonding) where

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
