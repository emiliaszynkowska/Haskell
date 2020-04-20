module ToyEval where
import ToyTokens
import ToyGrammar
------------------------------------------------------------
evalToy :: E -> E
evalToy e = eval [] e

eval :: Environment -> E -> E
eval env (Int int) = Int int
eval env (Var var) = Var var
eval env ToyTrue = ToyTrue
eval env ToyFalse = ToyFalse
eval env (Compare (Int x) (Int y)) 
    | x < y = ToyTrue
    | otherwise = ToyFalse
eval env (Plus (Int x) (Int y)) = Int (x + y)
eval env (If x y z)
    | (eval env x) == ToyTrue = eval env y
    | (eval env x) == ToyFalse = eval env z

eval env (Lambda x t e) = evaluate (update env (Var x) e) (Var x)

eval env (App (Plus (Var x) (Int y)) (Int z)) = eval env (Plus (Int y) (Int z))
eval env (App (Int z) (Plus (Var x) (Int y))) = eval env (Plus (Int y) (Int z))
eval env (App (Compare (Var x) (Int y)) (Int z)) = eval env (Compare (Int z) (Int y))
eval env (App (Int z) (Compare (Var x) (Int y))) = eval env (Compare (Int y) (Int z))

eval env (App (Int x) (Int y)) = App (Int x) (Int y)
eval env (App x (Int y)) = eval env (App (eval env x) (Int y))
eval env (App (Int y) x) = eval env (App (eval env x) (Int y))
eval env (App x y) = eval env (App (eval env x) (eval env y))

eval env (Let x t e1 e2) = evaluate (update env (Var x) (eval env e1)) (eval env e2)
eval env unknown = unknown 
------------------------------------------------------------
type Environment = [(E,E)]

update :: Environment -> E -> E -> Environment
update environment x e = (x,e) : environment

evaluate :: Environment -> E -> E
evaluate (env:environment) e
    | (fst env) == e = snd env
    | otherwise = eval environment e
------------------------------------------------------------