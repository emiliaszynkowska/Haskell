--Author: Julian Rathke, 2018 
--Provides an implementation of a type checker for the \Toy language from the lecture notes.
module ToyTypes where 
import ToyGrammar

--Data structures as defined in ToyGrammar:
--data ToyType = TyInt | TyBool | TyUnit | TyPair ToyType ToyType | TyFun ToyType ToyType
--type Environment = [ (String,Expr) ]
--data Expr = TmInt Int | TmTrue | TmFalse | TmUnit | TmCompare Expr Expr 
--           | TmPair Expr Expr | TmAdd Expr Expr | TmVar String 
--           | TmFst Expr | TmSnd Expr
--           | TmIf Expr Expr Expr | TmLet String ToyType Expr Expr
--           | TmLambda String ToyType Expr | TmApp Expr Expr
--           | Cl ( String ToyType Expr Environment)

type TypeEnvironment = [ (String,ToyType) ]

getBinding :: String -> TypeEnvironment -> ToyType
getBinding x [] = error "Variable binding not found"
getBinding x ((y,t):tenv) | x == y  = t
                        | otherwise = getBinding x tenv

addBinding :: String -> ToyType -> TypeEnvironment -> TypeEnvironment
addBinding x t tenv = (x,t):tenv

typeOf :: TypeEnvironment -> Expr -> ToyType
typeOf tenv (Int _)  = TypeInt

typeOf tenv (ToyTrue) = TypeBool

typeOf tenv (ToyFalse) = TypeBool

typeOf tenv (Compare e1 e2) = TypeBool
  where (TypeInt,TypeInt) = (typeOf tenv e1, typeOf tenv e2)

typeOf tenv (Add e1 e2) = TypeInt 
  where (TypeInt,TypeInt) = (typeOf tenv e1, typeOf tenv e2)

typeOf tenv (Var x) = getBinding x tenv

typeOf tenv (If e1 e2 e3) | t2 == t3 = t2
  where (TypeBool,t2,t3) = (typeOf tenv e1, typeOf tenv e2, typeOf tenv e3)

typeOf tenv (Lambda x t e) = TypeFun t u 
  where u = typeOf (addBinding x t tenv) e

typeOf tenv (App e1 e2) | t1 == t3 = t2
  where ((TypeFun t1 t2),t3) = (typeOf tenv e1, typeOf tenv e2)

typeOf tenv (Let x t e1 e2) | t == t1 = typeOf (addBinding x t tenv) e2
  where t1 = typeOf tenv e1

typeOf tenv _ = error "Type Error"

-- Function for printing the results of the TypeCheck
unparseType :: ToyType -> String
unparseType TypeBool = "Bool"
unparseType TypeInt = "Int"
unparseType (TypeFun t1 t2) = (unparseType t1) ++ " -> " ++ (unparseType t2)
