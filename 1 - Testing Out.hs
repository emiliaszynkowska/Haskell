--sums the first n squares
sumsq n = sum [ x^10 | x <- [0..n] ] 

--quicksort implementation
quicksort :: (Ord a) => [a] -> [a]                          --type definition
quicksort [] = []                                           --make an empty list
quicksort (x:xs) =                                          --for x in xs
    let smallerSorted = quicksort [a | a <- xs, a <= x]     --make a list smallerSorted, sort below x
        biggerSorted = quicksort [a | a <- xs, a > x]       --make a list biggerSorted, sort above x
    in  smallerSorted ++ [x] ++ biggerSorted                --concatenate the lists together
