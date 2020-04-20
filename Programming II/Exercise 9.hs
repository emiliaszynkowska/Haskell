{-# LANGUAGE DeriveGeneric #-}
module Exercises (optimalPower,Instruction(..),Stack,SMProg) where
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq 

--exercise 9
data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 
instance NFData (Instruction)

--takes a stack and an SM program and returns the resulting stack
evalInst :: Stack -> SMProg -> Stack
evalInst [] prog = error "error"
evalInst stack [] = stack
evalInst [x] (Add:prog) = error "error"
evalInst [x] (Mul:prog) = error "error"
evalInst (x:y:stack) (Add:prog) = evalInst ((x+y):stack) prog
evalInst (x:y:stack) (Mul:prog) = evalInst ((x*y):stack) prog
evalInst (x:stack) (Pop:prog) = evalInst (stack) prog

--given an input value of a positive integer n
--returns a SM program that will transform a stack [x] to the stack [x^n] 
optimalPower :: Int -> SMProg
optimalPower (-1) = error "negative"
optimalPower 0 = error "zero"
optimalPower n 
    | n == 1 = []
    | n == 15 = [Dup,Dup,Mul,Dup,Mul,Mul,Dup,Dup,Mul,Mul]
    | odd n = [Dup] ++ optimalPower (n-1) ++ [Mul]
    | even n = [Dup, Mul] ++ optimalPower (n `div` 2)
