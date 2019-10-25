module Exercises (approxSqrt) where

--exercise A2
--returns the square root using the Babylonian method
--stops when (d-c < epsilon) and returns the result
approxSqrt :: Double -> Double -> Double
approxSqrt 0 epsilon = 0
approxSqrt d epsilon = approxSqrt2 d epsilon d
approxSqrt2 :: Double -> Double -> Double -> Double
approxSqrt2 d epsilon c 
    | abs((sqrt d)-c) < epsilon = c
    | otherwise = approxSqrt2 d epsilon ((c+d/c)/2)

    
