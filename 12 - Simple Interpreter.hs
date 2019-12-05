data Expr = Var String | Lambda String Expr | App Expr Expr deriving (Eq, Show, Read)

subst :: Expr -> String -> Expr -> Expr
subst (Var output) x input
    | x == output = input
    | x /= output = Var output
subst (Lambda x output) input1 input2
    | x == input1 = Lambda x output
    | x /= input1 && not (free x input2) = Lambda x (subst output input1 input2)
    | x /= input1 && (free x input2) = 
        let x' = rename x in 
        subst (Lambda x' (subst output x (Var x'))) input1 input2 
subst (App x y) input1 input2 = App (subst x input1 input2) (subst y input1 input2)

free :: String -> Expr -> Bool
free x (Var y) = x == y
free x (Lambda y e) 
    | x == y = False
    | x /= y = free x e
free x (App e1 e2) = (free x e1) || (free x e2)

rename x = x ++ "\'"

evalcbn :: Expr -> Expr
evalcbn (Var x) = Var x
evalcbn (Lambda x output) = Lambda x output
evalcbn (App (Lambda x output) input) = subst output x input
evalcbn (App x y) = App (evalcbn x) y

evalcbv :: Expr -> Expr
evalcbv (Var x) = Var x
evalcbv (Lambda x output) = Lambda x output
evalcbv (App (Lambda x output1) input@(Lambda y output2)) = subst output1 x input
evalcbv (App function@(Lambda x output) input) = App function (evalcbv input)
evalcbv (App x y) = App (evalcbv x) y

