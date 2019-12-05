 -- COMP2209 Functional Programming Challenges
-- (c) University of Southampton 2019

module Challenges (alphaNorm, countAllReds, printLambda, parseLet, letToLambda,
    LamExpr(LamApp, LamAbs, LamVar), LetExpr(LetApp, LetDef, LetFun, LetVar, LetNum),
    lambdaToLet) where

import Data.Char
import Parsing

-- abstract data type for simple lambda calculus expressions
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int 
    deriving (Show, Eq)

-- abstract data type for simple let expressions
data LetExpr = LetApp LetExpr LetExpr  | LetDef [([Int], LetExpr)] LetExpr | LetFun Int | LetVar Int | LetNum Int deriving (Show, Eq)

-- Challenge 1
-- generate the alpha normal form for a simple lambda calculus expression
-- the expression is interpreted from left to right
alphaNorm :: LamExpr -> LamExpr
alphaNorm (LamVar x) = (LamVar 0)
alphaNorm (LamAbs x y) = (LamAbs 0 (alphaNorm2 x 1 y))
alphaNorm (LamApp x y) = (LamApp (alphaNorm2 0 0 x) (alphaNorm2 0 0 y)) 

alphaNorm2 :: Int -> Int -> LamExpr -> LamExpr
alphaNorm2 s i (LamVar x) 
    | x == s = (LamVar 0)
    | otherwise = (LamVar x)
alphaNorm2 s i (LamAbs x (LamVar y))
    | s /= 0 && y == 0 = (LamAbs 0 (LamVar 0))
    | x == y && x == s = (LamAbs 0 (LamVar 0))
    | x == y && x /= s = (LamAbs i (LamVar i))
    | x /= y && x == s = (LamAbs 0 (LamVar i))
    | x /= y && y == s = (LamAbs i (LamVar 0))
    | otherwise = (LamAbs i (LamVar (i+1))) 
alphaNorm2 s i (LamAbs x y) = (LamAbs i (alphaNorm2 s i y))
alphaNorm2 s i (LamApp x y) = (LamApp (alphaNorm2 s i x) (alphaNorm2 s i y)) 

-- Challenge 2
-- count all reduction paths for a given lambda expression m, of length up to a given limit l
countAllReds :: LamExpr -> Int -> Int
countAllReds expr limit = length (removeDuplicates (countReds expr 1 limit []))

removeDuplicates :: [LamExpr] -> [LamExpr]
removeDuplicates  [] = []
removeDuplicates (x:xs)   
                | x `elem` xs = removeDuplicates xs
                | otherwise = x : removeDuplicates xs

countReds :: LamExpr -> Int -> Int -> [LamExpr] -> [LamExpr]
countReds expr current limit list
    | current > limit               = []
    | expr /= left && expr /= right = list ++ (countReds left (current+1) limit list) ++ (countReds right (current+1) limit list)
    | expr /= left                  = list ++ (countReds left (current+1) limit list) 
    | expr /= right                 = list ++ (countReds right (current+1) limit list) 
    | otherwise                     = [expr]
    where left = evalleft expr
          right = evalright expr

-- Challenge 3 
-- pretty print a lambda expression, combining abstraction variables
-- also recognising Scott numerals and printing these as numbers
-- finalising omitting brackets where possible and safe to do so
printLambda :: LamExpr -> String
printLambda (LamApp (LamVar x) (LamVar y)) = "x" ++ show x ++ " x" ++ show y
printLambda (LamApp (LamAbs x (LamVar y)) (LamAbs z (LamVar w))) = "(\\x" ++ show x ++ " -> x" ++ show y ++ ") \\x" ++ show z ++ " -> x" ++ show w
printLambda (LamAbs x (LamApp (LamVar y) (LamAbs z (LamVar w)))) = "\\x" ++ show x ++ " -> x" ++ show y ++ " \\x" ++ show z ++ " -> x" ++ show w
printLambda (LamAbs x (LamAbs y (LamVar z))) = "0"
printLambda (LamAbs a (LamAbs b (LamApp (LamVar c) (LamAbs d (LamAbs e (LamVar f)))))) = "1"
printLambda expr = "unknown"

-- Challenge 4
-- parse let expressions

parseLet :: String -> Maybe LetExpr
parseLet input = case parse myParser input of
    [(a,[])] -> Just a
    [(_,string)] -> Nothing
    [] -> Nothing

myParser :: Parser LetExpr
myParser = defParser <|> appParserMul <|> appParser <|> bracketParser <|> varParser <|> funcParser

varParser :: Parser LetExpr
varParser = do 
            symbol "x"
            x <- nat
            return (LetVar x)

funcParser :: Parser LetExpr
funcParser = do 
            symbol "f"
            f <- nat
            return (LetFun f)

bracketParser :: Parser LetExpr
bracketParser = do 
                symbol "("
                input <- myParser
                symbol ")"
                return input

defParser :: Parser LetExpr
defParser = do
            symbol "let"
            eqn <- eqnListParser <|> eqnParser
            symbol "in"
            expr <- myParser
            return (LetDef eqn expr)

eqnListParser :: Parser [([Int], LetExpr)]
eqnListParser = do
                x <- eqnParser
                symbol ";"
                y <- eqnListParser <|> eqnParser
                return (x ++ y)

eqnParser :: Parser [([Int], LetExpr)]
eqnParser = do
            symbol "f"
            f <- nat
            xs <- listParser
            symbol "="
            expr <- myParser
            return [(f:xs,expr)]

listParser :: Parser [Int]
listParser = do
             x <- varListParser <|> singleVarParser
             return x

varListParser :: Parser [Int]
varListParser = do
                x <- singleVarParser
                y <- varListParser <|> singleVarParser
                return (x ++ y)

singleVarParser :: Parser [Int]
singleVarParser = do
                symbol "x"
                x <- nat 
                return [x]

appParser :: Parser LetExpr
appParser = do
    x <- varParser <|> funcParser <|> defParser <|> bracketParser
    y <- myParser
    return (LetApp x y)
appParserMul :: Parser LetExpr
appParserMul = do
    x <- varParser
    y <- varParser
    z <- myParser
    return (LetApp (LetApp x y) z)

-- Challenge 5
-- translate a let expression into lambda calculus, using Scott numerals
-- convert let symbols to lambda variables using Jansen's techniques rather than Y
letToLambda :: LetExpr -> LamExpr
letToLambda _ = LamVar (-1)
-- letToLambda (LetVar x)   = (LamVar x)
-- letToLambda (LetApp x y) = (LamApp (letToLambda x) (letToLambda y))
-- letToLambda (LetDef ())

-- convertLet :: Expr -> LamExpr
-- convertLet (Let (i:is) expr1 expr2) | length is == 0 = (LamApp (LamAbs i (convertLet expr2)) (convertLet expr1))
--                                     | otherwise      = (LamApp (LamAbs i (convertLet expr2)) (convertList is expr1))

-- -- --for a list of variables on lhs, builds required lamAbs using recursion 
-- -- --ie \2->(\3->(\4->x))
--convertList :: [Int] -> Expr -> LamExpr
-- convertList (i:is) expr | length is == 0 = (LamAbs i (convertLet expr))
--                         | otherwise      = (LamAbs i (convertList is expr)) 
-- LetDef [([Int], LetExpr)] LetExpr
-- ------------------------------------------------------------------------------------------
--  letToLambda (LetDef [([0],LetFun 0)] (LetFun 0)) ==
--           LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 0))) (LamAbs 0 (LamApp (LamVar 0) (LamVar 0))),
--         letToLambda (LetDef [([1,2],LetVar 2)] (LetFun 1)) ==
--           LamApp (LamAbs 0 (LamAbs 0 (LamVar 0))) (LamAbs 0 (LamAbs 0 (LamVar 0)))
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

-- Challenge 6
-- convert a lambda calculus expression into one using let expressions and application
lambdaToLet :: LamExpr -> LetExpr
lambdaToLet _ = LetVar (-1)

-- Extras
subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar output) x input
    | x == output = input
    | x /= output = LamVar output
subst (LamAbs x output) input1 input2
    | x == input1 = LamAbs x output
    | x /= input1 && not (free x input2) = LamAbs x (subst output input1 input2)
    | x /= input1 && (free x input2) = 
        let x' = rename x in 
        subst (LamAbs x' (subst output x (LamVar x'))) input1 input2 
subst (LamApp x y) input1 input2 = LamApp (subst x input1 input2) (subst y input1 input2)

free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e) 
    | x == y = False
    | x /= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

rename x = 2*x

evalleft :: LamExpr -> LamExpr
evalleft (LamVar x) = (LamVar x)
evalleft (LamAbs x output) = (LamAbs x output)
evalleft (LamApp (LamAbs x output) input) = subst output x input
evalleft (LamApp x y) = LamApp (evalleft x) y

evalright :: LamExpr -> LamExpr
evalright (LamVar x) = (LamVar x)
evalright (LamAbs x output) = (LamAbs x output)
evalright (LamApp (LamAbs x output) input) = subst output x input
evalright (LamApp x y) = LamApp x (evalright y)
