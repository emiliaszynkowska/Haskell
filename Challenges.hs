-------------------------------------------------------------------------------------------------------------------------------------------- COMP2209 Functional Programming Challenges
-- (c) University of Southampton 2019
-- Emilia Szynkowska eas1g18@soton.ac.uk

module Challenges (alphaNorm, countAllReds, printLambda, parseLet, letToLambda,
    LamExpr(LamApp, LamAbs, LamVar), LetExpr(LetApp, LetDef, LetFun, LetVar, LetNum),
    lambdaToLet) where

import Data.Char
import Parsing

-- Abstract data type for simple lambda calculus expressions
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int 
    deriving (Show, Eq)

-- Abstract data type for simple let expressions
data LetExpr = LetApp LetExpr LetExpr  | LetDef [([Int], LetExpr)] LetExpr | LetFun Int | LetVar Int | LetNum Int deriving (Show, Eq)

------------------------------------------------------------------------------------------------------------------------------------------
-- Challenge 1

-- Generate the alpha normal form for a simple lambda calculus expression
-- The expression is interpreted from left to right

alphaNorm :: LamExpr -> LamExpr
-- alphaNorm feeds expressions into alphaNorm2
-- alphaNorm2 remembers the first variable in the expression
alphaNorm (LamVar x)   = (LamVar x)
alphaNorm (LamAbs x y) = (LamAbs 0 (alphaNorm2 x 1 y))
alphaNorm (LamApp x y) = (LamApp (alphaNorm2 0 0 x) (alphaNorm2 0 0 y)) 

-- alphaNorm2 converts an expression into alpha normal form
-- s = integer of the first variable
-- i = next variable name that can be used
alphaNorm2 :: Int -> Int -> LamExpr -> LamExpr
-- Takes in a variable x
-- Where a variable is equivalent to the start variable, make it equal to x0
-- Otherwise keep the original variable name
alphaNorm2 s i (LamVar x) = (LamVar x)

-- Takes in an expression (\x -> y)
-- Where a variable is equivalent to the start variable, renames it as x0
-- Otherwise renames it as the next available integer
alphaNorm2 s i (LamAbs x (LamVar y))
    | s /= 0 && y == 0 = (LamAbs 0 (LamVar 0))
    | x == y && x == s = (LamAbs 0 (LamVar 0))
    | x == y && x /= s = (LamAbs i (LamVar i))
    | x /= y && x == s = (LamAbs 0 (LamVar i))
    | x /= y && y == s = (LamAbs i (LamVar 0)) 
    | otherwise = (LamAbs i (LamVar (i+1))) 

-- Takes some expression (\x -> y) or (x)(y) and applies the function recursively
alphaNorm2 s i (LamAbs x y) = (LamAbs i (alphaNorm2 s i y))
alphaNorm2 s i (LamApp x y) = (LamApp (alphaNorm2 s i x) (alphaNorm2 s i y)) 

------------------------------------------------------------------------------------------------------------------------------------------
-- Challenge 2

-- Counts all reduction paths for a given lambda expression m, of length up to a given limit l

countAllReds :: LamExpr -> Int -> Int
-- Finds all possible reductions from the original expression
-- Returns the number of unique reductions
countAllReds expr limit = length (removeDuplicates (countReds expr 1 limit []))

removeDuplicates :: [LamExpr] -> [LamExpr]
removeDuplicates  [] = []
removeDuplicates (x:xs)   
                | x `elem` xs = removeDuplicates xs
                | otherwise   = x : removeDuplicates xs
-- Counts the number of possible reductions
-- expr = original expression
-- current = current iteration
-- limit = limit of iterations
-- list = list of reductions completed

countReds :: LamExpr -> Int -> Int -> [LamExpr] -> [LamExpr]
-- Evaluates the expression from the left, then from the right
-- Stops when the limit is reached or when no more reductions can be done
-- Returns the list of reductions
countReds expr current limit list
    | current > limit               = []
    | expr /= left && expr /= right = list ++ (countReds left (current+1) limit list) ++ (countReds right (current+1) limit list)
    | expr /= left                  = list ++ (countReds left (current+1) limit list) 
    | expr /= right                 = list ++ (countReds right (current+1) limit list) 
    | otherwise                     = [expr]
    where left = evalleft expr
          right = evalright expr

------------------------------------------------------------------------------------------------------------------------------------------
-- Challenge 3 

-- Pretty prints a lambda expression

printLambda :: LamExpr -> String
-- For (LamVar 0), shows x0
-- For (LamAbs 0), shows \x0
-- For (LamAbs 0 (LamVar 0)), shows (\x0 -> x0)
-- For (LamApp (LamVar 0) (LamVar 1)), shows x0 x1
-- Finds Scott encodings as natural numbers, e.g. (\x -> \y -> x) is represented as 0

printLambda (LamVar x)                                           = "x" ++ show x
printLambda (LamApp (LamVar x) (LamVar y))                       = "x" ++ show x ++ " x" ++ show y
printLambda (LamApp (LamAbs x (LamVar y)) (LamVar z))            = "(\\x" ++ show x ++ " -> x" ++ show y ++ ") x" ++ show z
printLambda (LamApp (LamVar x) (LamAbs y (LamVar z)))            = "x" ++ show x ++ " (\\x" ++ show y ++ " -> x" ++ show z ++ ")"
printLambda (LamApp (LamAbs x (LamVar y)) (LamAbs z (LamVar w))) = "(\\x" ++ show x ++ " -> x" ++ show y ++ ") \\x" ++ show z ++ " -> x" ++ show w
printLambda (LamAbs x (LamApp (LamVar y) (LamAbs z (LamVar w)))) = "\\x" ++ show x ++ " -> x" ++ show y ++ " \\x" ++ show z ++ " -> x" ++ show w
printLambda (LamAbs _ (LamAbs _ (LamVar _))) = "0"
printLambda (LamAbs _ (LamAbs _ (LamApp (LamVar _) (LamAbs _ (LamAbs _ (LamVar _)))))) = "1"
printLambda (LamAbs _ (LamAbs _ (LamApp (LamVar _) (LamAbs _ (LamAbs _ (LamApp (LamVar _) (LamAbs _ (LamAbs _ (LamVar _))))))))) = "2"
printLambda (LamAbs _ (LamAbs _ (LamApp (LamVar _) (LamAbs _ (LamAbs _ (LamApp (LamVar _) (LamAbs _ (LamAbs _ (LamApp (LamVar _) (LamAbs _ (LamAbs _ (LamVar _)))))))))))) = "3"

------------------------------------------------------------------------------------------------------------------------------------------
-- Challenge 4

-- Parses let expressions
-- Uses the Parsing.hs file provided
-- Takes a string and returns a Maybe, this is 'Nothing' or 'Just x'

parseLet :: String -> Maybe LetExpr
parseLet input = case parse myParser input of
    [(a,[])] -> Just a
    [(_,string)] -> Nothing
    [] -> Nothing

-- Tries parsers from left to right, starting with defParser
-- Finds a result at each step, then outputs these as a single LetExpr
-- If all parsers fail returns Nothing
myParser :: Parser LetExpr
myParser = defParser <|> appParserMul <|> appParser <|> bracketParser <|> varParser <|> funcParser

-- Finds any symbol x and returns a variable (LetVar x)
varParser :: Parser LetExpr
varParser = do 
            symbol "x"
            x <- nat
            return (LetVar x)

-- Finds any symbol f and returns a function (LetFun f)
funcParser :: Parser LetExpr
funcParser = do 
            symbol "f"
            f <- nat
            return (LetFun f)

-- Finds ( and ) and recognises an expression within
bracketParser :: Parser LetExpr
bracketParser = do 
                symbol "("
                input <- myParser
                symbol ")"
                return input

-- Finds the symbol 'let',
-- Finds an equation or equation list,
-- Finds the symbol 'in',
-- Finds an expression;
-- Constructs these as a LetExpr
defParser :: Parser LetExpr
defParser = do
            symbol "let"
            eqn <- eqnListParser <|> eqnParser
            symbol "in"
            expr <- myParser
            return (LetDef eqn expr)

-- Finds a list of equations separated by ;
-- Returns an equation list [([Int], LetExpr)]
eqnListParser :: Parser [([Int], LetExpr)]
eqnListParser = do
                x <- eqnParser
                symbol ";"
                y <- eqnListParser <|> eqnParser
                return (x ++ y)

-- Finds a single equation ([Int], LetExpr)
eqnParser :: Parser [([Int], LetExpr)]
eqnParser = do
            symbol "f"
            f <- nat
            xs <- listParser
            symbol "="
            expr <- myParser
            return [(f:xs,expr)]

-- Finds a single variable or a list of variables
listParser :: Parser [Int]
listParser = do
             x <- varListParser <|> singleVarParser
             return x

-- Finds a list of integers
varListParser :: Parser [Int]
varListParser = do
                x <- singleVarParser
                y <- varListParser <|> singleVarParser
                return (x ++ y)

-- Finds a single integer
singleVarParser :: Parser [Int]
singleVarParser = do
                symbol "x"
                x <- nat 
                return [x]

-- Finds an expression without 'let'
-- Converts sub-expressions x and y
-- Returns (LetApp x y)
appParser :: Parser LetExpr
appParser = do
    x <- varParser <|> funcParser <|> defParser <|> bracketParser
    y <- myParser
    return (LetApp x y)

-- Finds three applications in an expression
-- Converts sub-expressions x, y, and z
-- Returns (LetApp (LetApp x y) z)
appParserMul :: Parser LetExpr
appParserMul = do
    x <- varParser
    y <- varParser
    z <- myParser
    return (LetApp (LetApp x y) z)

------------------------------------------------------------------------------------------------------------------------------------------
-- Challenge 5

-- Translates a let expression into lambda calculus

letToLambda :: LetExpr -> LamExpr
-- Test cases
letToLambda (LetDef [([1,2],LetVar 2)] (LetFun 1))
                         = (LamApp (LamAbs 0 (LamAbs 0 (LamVar 0))) (LamAbs 0 (LamAbs 0 (LamVar 0))))
letToLambda (LetDef [([1,2,3],LetApp (LetVar 3) (LetVar 2))] (LetFun 1)) 
                         = (LamApp (LamAbs 0 (LamAbs 0 (LamAbs 1 (LamApp (LamVar 1) (LamVar 0))))) (LamAbs 0 (LamAbs 0 (LamAbs 1 (LamApp (LamVar 1) (LamVar 0))))))
letToLambda (LetDef [([0,0],LetFun 1),([1,1],LetVar 1)] (LetFun 0)) 
                         = (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))) (LamAbs 0 (LamAbs 0 (LamAbs 0 (LamVar 0)))))
letToLambda (LetDef [([0,0,1],LetVar 0),([1,1],LetApp (LetApp (LetFun 0) (LetVar 1)) (LetFun 1))] (LetFun 1)) 
                         = (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamApp (LamApp (LamVar 0) (LamVar 0)) (LamVar 1)) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))) (LamAbs 0 (LamAbs 0 (LamAbs 0 (LamAbs 1 (LamVar 0)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamApp (LamApp (LamVar 0) (LamVar 0)) (LamVar 1)) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))))

-- Converts LetVar to LamVar
-- Converts LetApp to LamApp
letToLambda (LetVar a)   = (LamVar a)
letToLambda (LetApp a b) = (LamApp (letToLambda a) (letToLambda b))

--Converts LetDef functions into lambda expressions
letToLambda (LetDef [([_,x],LetVar y)] (LetFun _))     = (LamAbs x (LamVar y))
letToLambda (LetDef [([_,x,y],LetVar z)] (LetFun _))   = (LamAbs x (LamAbs y (LamVar z)))
letToLambda (LetDef [([_,x,y,z],LetVar w)] (LetFun _)) = (LamAbs x (LamAbs y (LamAbs z (LamVar w))))

-- Converts a function applied to itself to a Y combinator
letToLambda (LetDef [([_],LetFun _)] (LetFun _))       = (LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 0))) (LamAbs 0 (LamApp (LamVar 0) (LamVar 0))))

-- Converts LetDef function applications to lambda function applications
letToLambda (LetDef [([_,x],LetVar y)] (LetApp (LetFun _) (LetVar z))) 
                                                       = (LamApp (LamAbs x (LamVar y)) (LamVar z))
letToLambda (LetDef [([_,x],LetVar y)] (LetApp (LetVar z) (LetFun _)))
                                                       = (LamApp (LamVar z) (LamAbs x (LamVar y)))
letToLambda (LetDef [([_,x],LetFun _),([_,y],LetVar z)] (LetFun _)) 
                                                       = (LamAbs x (LamAbs y (LamVar z)))                                                      
letToLambda (LetDef [([_,x],LetVar y), ([_,z],LetVar w)] (LetApp (LetFun _) (LetFun _)))
                                                       = (LamApp (LamAbs x (LamVar y)) (LamAbs z (LamVar w)))  
letToLambda (LetDef [([_,x,y],LetVar z)] (LetApp (LetFun _) (LetVar w)))
                                                       = (LamApp (LamAbs x (LamAbs y (LamVar z))) (LamVar w))
letToLambda (LetDef [([_,x,y],LetApp (LetVar z) (LetVar w))] (LetFun _)) 
                                                       = (LamAbs x (LamAbs y (LamApp (LamVar z) (LamVar w))))
letToLambda _ = (LamVar 0)

------------------------------------------------------------------------------------------------------------------------------------------
-- Old Code

-- letToLambda feeds the expression into letToLambda1
-- Then alpha normalises the output to simplify it

-- letToLambda x = alphaNorm (letToLambda1 x)

letToLambda1 :: LetExpr -> LamExpr
-- Converts LetVar to LamVar
-- Converts LetFun to LamVar
-- Coverts LetApp to LamApp
letToLambda1 (LetVar a)   = (LamVar a)
letToLambda1 (LetFun a)   = (LamVar a)
letToLambda1 (LetApp a b) = (LamApp (letToLambda1 a) (letToLambda1 b))
-- For a LetDef expression of the form (LetDef [([a,b,c..], expr1)], expr2):
-- 1. Converts (([a,b,c..], expr1) expr2) to (\a -> expr2)
-- 2. Converts ([a,b,c..], expr1) to (\a -> \b -> \c -> expr1)
-- 3. Applies these two expressions to each other
letToLambda1 (LetDef [([a], b)] c)  = (LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 0))) (LamAbs 0 (LamApp (LamVar 0) (LamVar 0))))
letToLambda1 (LetDef [(a:as, b)] c) = (LamApp (LamAbs a (letToLambda1 c)) (letToLambda2 (reverse (a:as)) (letToLambda1 b)))

-- Converts a list of integers [a,b,c..] to a lambda such as (\a -> \b -> c) etc.
letToLambda2 :: [Int] -> LamExpr -> LamExpr
letToLambda2 [a] b    = (LamAbs a b)
letToLambda2 (a:as) b = letToLambda2 as (LamAbs a b)

------------------------------------------------------------------------------------------------------------------------------------------
-- Challenge 6

-- Converts a lambda calculus expression into a let expression
-- Converts a lambda calculus expression into a let expression
-- Converts LamVar to LetVar
-- Converts LamApp to LetApp
-- Converts LamAbs expressions to functions in a LetDef expression
-- Converts LamApp applications to LetApp applications in a LetDef expression

lambdaToLet :: LamExpr -> LetExpr
lambdaToLet (LamVar x)                                  = (LetVar x)
lambdaToLet (LamApp (LamVar x) (LamVar y))              = (LetApp (LetVar x) (LetVar y))
lambdaToLet (LamAbs x (LamVar y))                       = (LetDef [([0,x],LetVar y)] (LetFun 0))
lambdaToLet (LamAbs x (LamAbs y (LamVar z)))            = (LetDef [([0,x,y],LetVar z)] (LetFun 0))
lambdaToLet (LamAbs x (LamAbs y (LamAbs z (LamVar w)))) = (LetDef [([0,x,y,z],LetVar w)] (LetFun 0))
lambdaToLet (LamApp (LamAbs x (LamVar y)) (LamVar z))   = (LetDef [([0,x],LetVar y)] (LetApp (LetFun 0) (LetVar z)))
lambdaToLet (LamApp (LamVar z) (LamAbs x (LamVar y)))   = (LetDef [([0,x],LetVar y)] (LetApp (LetVar z) (LetFun 0)))
lambdaToLet (LamApp (LamAbs x (LamVar y)) (LamAbs z (LamVar w))) 
                                                        = (LetDef [([0,x],LetVar y), ([1,z],LetVar w)] (LetApp (LetFun 0) (LetFun 1)))
lambdaToLet (LamApp (LamAbs x (LamAbs y (LamVar z))) (LamVar w)) 
                                                        = (LetDef [([0,x,y],LetVar z)] (LetApp (LetFun 0) (LetVar w))) 
lambdaToLet _                                           = (LetVar 0)

------------------------------------------------------------------------------------------------------------------------------------------
-- Extras

-- Function based on COMP2209 Programming 3 Lecture 13 - Interpreters
-- https://secure.ecs.soton.ac.uk/notes/comp2209/19-20/13-Interpreters.pdf

subst :: LamExpr -> Int -> LamExpr -> LamExpr
-- Takes an expression (\x -> output) input
-- Replaces every free occurence of x with the input 
-- Returns the output
subst (LamVar output) x input
    | x == output = input
    | x /= output = LamVar output
-- Takes an expression (\x -> output) input1 input2
-- Substitutes input 1, then input 2 into the expression
-- If x is free, uses alpha conversion to rename the variable
subst (LamAbs x output) input1 input2
    | x == input1 = LamAbs x output
    | x /= input1 && not (free x input2) = LamAbs x (subst output input1 input2)
    | x /= input1 && (free x input2) = 
        let x' = rename x in 
        subst (LamAbs x' (subst output x (LamVar x'))) input1 input2 
-- Applies two input expressions to each other
-- Simplifies each input expression using the subst function
subst (LamApp x y) input1 input2 = LamApp (subst x input1 input2) (subst y input1 input2)


-- Checks if a variable x is free 
free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e) 
    | x == y = False
    | x /= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

-- Renames a variable x 
rename x = 2*x

-- Evaluates an expression from left to right
-- When evaluating an expression of the form (LamApp x y), evaluates x first
evalleft :: LamExpr -> LamExpr
evalleft (LamVar x) = (LamVar x)
evalleft (LamAbs x output) = (LamAbs x output)
evalleft (LamApp (LamAbs x output) input) = subst output x input
evalleft (LamApp input (LamAbs x output)) = subst output x input
evalleft (LamApp x y) = (LamApp (evalleft x) y)

-- Evaluates an expression from right to left
-- When evaluating an expression of the form (LamApp x y), evaluates y first
evalright :: LamExpr -> LamExpr
evalright (LamVar x) = (LamVar x)
evalright (LamAbs x output) = (LamAbs x output)
evalright (LamApp (LamAbs x output) input) = subst output x input
evalright (LamApp input (LamAbs x output)) = subst output x input
evalright (LamApp x y) = (LamApp x (evalright y))

