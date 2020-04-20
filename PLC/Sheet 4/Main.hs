import System.Environment (getArgs)
import Control.Exception
import ToyTokens
import ToyGrammar
import ToyEval

errorCall :: ErrorCall -> IO()
errorCall error = putStr (show error)

main :: IO()
main = catch main2 errorCall
main2 = do 
    [file] <- getArgs
    readFile file >>= print . evalToy . parseToy . alexScanTokens 

-- to run:
-- :main input.txt