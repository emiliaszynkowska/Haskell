{-# LANGUAGE DeriveGeneric #-}
module Exercises (evalInst,Instruction(..),Stack,SMProg) where
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

--exercise 7 
--add pops the top two elements of the stack, adds them and pushes the result back on to the stack
--mul pops the top two elements of the stack, multiplies them and pushes the result back on to the stack
--dup pushes another copy of the top element on to the stack
--pop removes the top element of the stack
data Instruction = Add | Mul | Dup | Pop
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
evalInst (x:stack) (Dup:prog) = evalInst (x:x:stack) prog
evalInst (x:stack) (Pop:prog) = evalInst (stack) prog