module Exercises (approxSqrt) where

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
