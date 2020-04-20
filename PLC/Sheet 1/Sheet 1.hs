import Data.Char
import Data.List.Split
import qualified Data.ByteString.Lazy as BL
--Sheet 1
--Task 1 & 2
zipL :: [a] -> [a] -> [[a]]
zipL [x] [y] = [x,y] : []
zipL [x] [] = [x] : []
zipL [] [y] = [y] : []
zipL (x:xs) [] = [x] : zipL xs []
zipL [] (y:ys) = [y] : zipL [] ys
zipL (x:xs) (y:ys) = [x,y] : zipL xs ys

unzipL :: [[a]] -> ([a],[a])
unzipL xss = (findFirst xss,findSecond xss)
findFirst [] = []
findFirst (xs:xss) = (head xs) : findFirst xss 
findSecond [] = []
findSecond (xs:xss) = (last xs) : findSecond xss

--Task 3
multiZipL :: [[a]] -> [[a]]
multiZipL ([]:xss) = []
multiZipL xss = (findHeads xss) : (multiZipL (findTails xss))

findHeads :: [[a]] -> [a]
findHeads xss = map head xss

findTails :: [[a]] -> [[a]]
findTails xss = map tail xss

--Task 4: Haskell Output
multiZipF :: IO ()
multiZipF = do 
    csvData <- BL.readFile "input.csv"
    let splitInput = splitOn "\\r\\n" (show csvData)
    let listInput = makeList splitInput
    let listOutput = multiZipL listInput
    print (listOutput)

makeList :: [String] -> [[Int]]
makeList [] = []
makeList (xs:xss) = map toInt (filter (\x -> isDigit x) xs) : makeList xss
                    where toInt = (\x -> read [x]::Int)

--Task 4: CSV Output
multiZipF' :: IO ()
multiZipF' = do 
    csvData <- BL.readFile "input.csv"
    let splitInput = splitOn "\\r\\n" (show csvData)
    let listInput = makeList' splitInput
    let listOutput = multiZipL listInput
    makeOutput listOutput

makeList' :: [String] -> [[Int]]
makeList' [] = []
makeList' (xs:xss) = map toInt (filter (\x -> isDigit x) xs) : makeList' xss
                     where toInt = (\x -> read [x]::Int)

makeOutput :: [[Int]] -> IO ()
makeOutput xs = do let output = concat $ map listToString xs
                   writeFile "output.csv" output
k
listToString :: [Int] -> String
listToString xs = (concat $ map stringFunction xs) ++ "\n"
                    where stringFunction = (\x -> (show x) ++ ",")