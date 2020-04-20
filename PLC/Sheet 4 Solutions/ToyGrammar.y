{ 
module ToyGrammar where 
import ToyTokens 
}

%name parseCalc 
%tokentype { ToyToken } 
%error { parseError }
%token 
    Bool   { TokenTypeBool _ } 
    Int    { TokenTypeInt _ } 
    arrow  { TokenArrow _ } 
    int    { TokenInt _ $$ } 
    true   { TokenToyTrue _ }
    false  { TokenToyFalse _ }
    '<'    { TokenLessThan _ }
    '+'    { TokenPlus _ }
    var    { TokenVar _ $$ }
    if     { TokenIf _ }
    then   { TokenThen _ }
    else   { TokenElse _ }
    lam    { TokenLambda _ }
    let    { TokenLet _ }
    ':'    { TokenCons _ }
    '='    { TokenEq _ }
    in     { TokenIn _ }
    '('    { TokenLParen _ } 
    ')'    { TokenRParen _ } 

%left arrow lam '<' '+' APP
%right let in
%nonassoc if then else int true false var '(' ')'

%% 
Exp : int                                       { Int $1 } 
    | var                                       { Var $1 }
    | true                                      { ToyTrue }
    | false                                     { ToyFalse } 
    | Exp '<' Exp                               { Compare $1 $3 } 
    | Exp '+' Exp                               { Add $1 $3 }
    | if Exp then Exp else Exp                  { If $2 $4 $6 } 
    | lam '(' var ':' Type ')' Exp              { Lambda $3 $5 $7 }
    | let '(' var ':' Type ')' '=' Exp in Exp   { Let $3 $5 $8 $10 }
    | Exp Exp %prec APP                         { App $1 $2 } 
    | '(' Exp ')'                               { $2 }

Type : Bool            { TypeBool } 
     | Int             { TypeInt } 
     | Type arrow Type { TypeFun $1 $3 } 


{ 
parseError :: [ToyToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data ToyType = TypeInt | TypeBool | TypeFun ToyType ToyType
   deriving (Show,Eq)

type Environment = [(String,Expr)]

data Expr = Int Int | ToyTrue | ToyFalse | Compare Expr Expr 
            | Add Expr Expr | Var String 
            | If Expr Expr Expr | Let String ToyType Expr Expr
            | Lambda String ToyType Expr | App Expr Expr 
            | Cl String ToyType Expr Environment
    deriving (Show,Eq)
} 