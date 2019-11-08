module Exercises (findBonding) where
import qualified Data.List as S

--exercise 5
--p = predicate, xs = input list
--function B to S is a bonding if it is total and symmetric
--findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding p xs = findSymmetry(sortAllPairs(findAllPairs p xs))

--finds all possible pairs which satisfy the predicate
findAllPairs :: Eq a => (a -> a -> Bool) -> [a] -> [(a,a)]
findAllPairs p xs = [(x,y) | x <- xs, y <- xs, p x y == True]
--sorts the result of findAllPairs
sortAllPairs xys = S.sort xys
--finds symmetric pairs
findSymmetry xys = [[(a,b),(c,d)] | a == x, b == y, c == x, d == y, a == d, b == c, a /= b, c /= d]



