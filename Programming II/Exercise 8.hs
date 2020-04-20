{-# LANGUAGE DeriveGeneric #-}
module Exercises (findMaxReducers,Instruction(..),Stack,SMProg) where
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

--exercise 8
--add pops the top two elements of the stack, adds them and pushes the result back on to the stack
--mul pops the top two elements of the stack, multiplies them and pushes the result back on to the stack
--pop removes the top element of the stack
data Instruction = Add | Mul | Pop deriving (Eq,Ord,Show,Generic)
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

--a maximal reducer returns the highest possible value for that input stack
--returns all maximal reducers for the given input stack
findMaxReducers :: Stack -> [SMProg]
findMaxReducers [] = []
findMaxReducers stack = [prog | prog <- instCombos, head (evalInst stack prog) == maxResult stack instCombos]
  where instCombos = allInstCombos ((length stack)-1)

-- Find maximum result sequence of instructions combination can produce on a stack
maxResult :: Stack -> [SMProg] -> Int
maxResult stack progs = maximum [head (evalInst stack prog) | prog <- progs]
 
-- Return all combinations thereof for a given sequence of instructions
allInstCombos :: Int -> [SMProg]
allInstCombos 0 = [[]]
allInstCombos n = map ([Add]++) (allInstCombos (n-1)) ++ map ([Mul]++) (allInstCombos (n-1)) ++ map ([Pop]++) (allInstCombos (n-1))