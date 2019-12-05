module CEK where

--C   --Control
--E   --Environment
--(S) --Store
--K   --Continuation

data Exp = Var String | Lam String Exp | App Exp Exp deriving Show

newtype Env = Env { (!) :: String -> Value }

instance Show Env where
  show _ = "Env"

data Value = Closure String Exp Env

data Kont = Top | Arg Exp Env Kont | Fun String Exp Env Kont deriving Show

data State = State Exp Env Kont deriving Show

start :: Exp -> State
start c = State c (Env $ const undefined) Top

id_ = Lam "x" $ Var "x"
const_ = Lam "x" $ Lam "y" $ Var "x"

-- small-step semantics step
step :: State -> State
step s@(State c e k) = case c of
  Var v -> case e ! v of
    Closure v' b e' -> State (Lam v' b) e' k
  App cf cx -> State cf e (Arg cx e k)
  Lam v b -> case k of
    Top -> s
    Arg cx e' k' -> State cx e' (Fun v b e k')
    Fun v' b' e' k' -> State b' (extend v' (Closure v b e) e') k'

extend :: String -> Value -> Env -> Env
extend i v f = Env $ \j -> if i == j then v else f ! j

final :: State -> Bool
final (State Lam{} _ Top) = True
final _ = False

eval :: State -> State
eval = until final step 