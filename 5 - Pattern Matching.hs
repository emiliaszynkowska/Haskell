not :: Bool -> Bool
not False = True
not True = False

nonZero :: Int -> Bool
nonzero 0 = False
nonZero _ = True

(&&) :: Bool -> Bool -> Bool
True && b = b
False && _ = False

first :: (Int,Int) -> Int
first (x,_) = x
second :: (Int,Int) -> Int
second (_,y) = y

last [] = error "empty list"
last (x:[]) = x
last (x:xs) = last xs

fetch :: Int -> [Int] -> Int
fetch _ [] = error "empty list"
fetch 0 (x:_) = x
fetch n (_:xs) = fetch (n-1) xs

toUpperStr :: String -> String 
toUpperStr [] = [] 
toUpperStr (c:cs) = toUpper c : toUpperStr cs


