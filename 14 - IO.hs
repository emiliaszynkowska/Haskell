--getChar :: IO Char 
--putChar :: Char -> IO() 
--putStr :: String -> IO()
--putStrLn :: String -> IO() 
--readFile :: FilePath -> IO String
--writeFile :: FilePath -> String -> IO()
--appendFile :: FilePath -> String -> IO()
--randomRIO :: (a,a) -> IO a

getPair :: IO(Char,Char)
getPair = do 
    x <- getChar
    getChar
    y <- getChar
    return (x,y)

hangman :: IO()
hangman = do 
    putStrLn "Think of a word..."
    word <- secretGetLine
    let spaces = replicate (length word) '-'
    putStrLn spaces
    putStrLn "Try to guess it..."
    play word spaces
secretGetLine :: IO String
secretGetLine = do
    xs <- getLine 
    return xs
play :: String -> String -> IO()
play word currentAnswer 
    | currentAnswer == word = putStrLn "Correct!"
    | otherwise = do
        putStrLn "Enter a character..."
        guess <- getChar
        newAnswer <- putUpdate(updateMatch word currentAnswer guess)
        play word newAnswer
putUpdate :: String -> IO String
putUpdate s = do 
    putStr "Your answer so far is: "
    putStrLn s 
    return s 
updateMatch :: String -> String -> Char -> String
updateMatch [] [] guess = []
updateMatch (x:xs) (y:ys) g
    | x == y = x : updateMatch xs ys g
    | x == g = x : updateMatch xs ys g 
    | otherwise = '-' : updateMatch xs ys g