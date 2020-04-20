-- Author: Julian Rathke, 2018
-- Provides a CEK implementation of the \Toy language from the lecture notes
module ToyEval where
import ToyGrammar

--Data structures as defined in ToyGrammar:
--data ToyType = TypeInt | TypeBool | TypeFun ToyType ToyType
--type Environment = [(String,Expr)]
--data Expr = Int Int | ToyTrue | ToyFalse | Compare Expr Expr 
--           | Add Expr Expr | Var String 
--           | If Expr Expr Expr | Let String ToyType Expr Expr
--           | Lambda String ToyType Expr | App Expr Expr
--           | Cl (String ToyType Expr Environment)

data Frame = HCompare Expr Environment 
           | CompareH Expr
           | HAdd Expr Environment | AddH Expr
           | HIf Expr Expr Environment | HLet String ToyType Expr Environment
           | HApp Expr Environment | AppH Expr
type Kontinuation = [Frame]
type State = (Expr,Environment,Kontinuation)

-- Function to unpack a closure to extract the underlying lambda term and environment
unpack :: Expr -> (Expr,Environment)
unpack (Cl x t e env1) = ((Lambda x t e) , env1)
unpack e = (e,[])

-- Look up a value in an environment and unpack it
getValueBinding :: String -> Environment -> (Expr,Environment)
getValueBinding x [] = error "Variable binding not found"
getValueBinding x ((y,e):env) | x == y  = unpack e
                              | otherwise = getValueBinding x env

update :: Environment -> String -> Expr -> Environment
update env x e = (x,e) : env

-- Checks for terminated expressions
isValue :: Expr -> Bool
isValue (Int _) = True
isValue ToyTrue = True
isValue ToyFalse = True
isValue (Cl _ _ _ _) = True
isValue _ = False

--Small step evaluation function
eval1 :: State -> State
eval1 ((Var x),env,k) = (e',env',k) 
                    where (e',env') = getValueBinding x env

-- Rule for terminated evaluations
eval1 (v,env,[]) | isValue v = (v,env,[])
                  
-- Evaluation rules for less than operator
eval1 ((Compare e1 e2),env,k) = (e1,env,(HCompare e2 env):k)
eval1 ((Int n),env1,(HCompare e env2):k) = (e,env2,(CompareH (Int n)) : k)
eval1 ((Int m),env,(CompareH (Int n)):k) | n < m = (ToyTrue,[],k)
                                             | otherwise = (ToyFalse,[],k)

-- Evaluation rules for plus operator
eval1 ((Add e1 e2),env,k) = (e1,env,(HAdd e2 env):k)
eval1 ((Int n),env1,(HAdd e env2):k) = (e,env2,(AddH (Int n)) : k)
eval1 ((Int m),env,(AddH (Int n)):k) = (Int (n + m),[],k)

-- Evaluation rules for if-then-else
eval1 ((If e1 e2 e3),env,k) = (e1,env,(HIf e2 e3 env):k)
eval1 (ToyTrue,env1,(HIf e2 e3 env2):k) = (e2,env2,k)
eval1 (ToyFalse,env1,(HIf e2 e3 env2):k) = (e3,env2,k)

-- Evaluation rules for Let blocks
eval1 ((Let x typ e1 e2),env,k) = (e1,env,(HLet x typ e2 env):k)
eval1 (v,env1,(HLet x typ e env2):k) | isValue v = (e, update env2 x v , k)

--  Rule to make closures from lambda abstractions.
eval1 ((Lambda x typ e),env,k) = ((Cl x typ e env), [], k)

-- Evaluation rules for application
eval1 ((App e1 e2),env,k) = (e1,env, (HApp e2 env) : k)
eval1 (v,env1,(HApp e env2):k ) | isValue v = (e, env2, (AppH v) : k)
eval1 (v,env1,(AppH (Cl x typ e env2) ) : k )  = (e, update env2 x v, k)

-- Rule for runtime errors
eval1 (e,env,k) = error "Evaluation Error"

-- Function to iterate the small step reduction to termination
evalLoop :: Expr -> Expr 
evalLoop e = evalLoop' (e,[],[])
  where evalLoop' (e,env,k) = if (e' == e) && (isValue e') && (null k) then e' else evalLoop' (e',env',k')
                       where (e',env',k') = eval1 (e,env,k) 

-- Function to unparse underlying values from the AST term
unparse :: Expr -> String 
unparse (Int n) = show n
unparse (ToyTrue) = "true"
unparse (ToyFalse) = "false"
unparse (Cl _ _ _ _) = "Function Value"
unparse _ = "Unknown"
