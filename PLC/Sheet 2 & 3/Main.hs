import System.Environment (getArgs)
import Control.Exception
import MDLTokens
import MDLGrammar

errorCall :: ErrorCall -> IO()
errorCall error = putStr (show error)

main :: IO()
main = catch main2 errorCall
main2 = do 
    [file] <- getArgs
    readFile file >>= print . parseMDL . alexScanTokens 

-- to run:
-- :main input.txt