{
module ToyGrammar where
import ToyTokens
}

%name parseToy
%tokentype { Token }
%error { parseError }

%token
    let     { TokenLet      }
    in      { TokenIn       }
    if      { TokenIf       }
    then    { TokenThen     }
    else    { TokenElse     }
    integer { TokenInteger  }
    bool    { TokenBool     }
    lam     { TokenLam      }
    true    { TokenToyTrue  }
    false   { TokenToyFalse }
    int     { TokenInt $$   }
    var     { TokenVar $$   }
    app     { TokenApp      }
    '='     { TokenEq       }
    '+'     { TokenPlus     }
    '<'     { TokenCompare  }
    '('     { TokenLParen   }
    ')'     { TokenRParen   }
    ':'     { TokenCons     }
    '\\'    { TokenLambda   }
    '->'    { TokenArrow    }

%right '//' '->' app 
%left '+'
%nonassoc '<' else in let
%%

E : int                            { Int $1           }
  | var                            { Var $1           } 
  | true                           { ToyTrue          }
  | false                          { ToyFalse         }
  | E '<' E                        { Compare $1 $3    } 
  | E '+' E                        { Plus $1 $3       }
  | if E then E else E             { If $2 $4 $6      }
  | '\\''('var':'T')' '->' E       { Lambda $3 $5 $8  }
  | app '('E')' '('E')'            { App $3 $6        }
  | let '('var':'T')' '=' E in E   { Let $3 $5 $8 $10 } 

T : integer                        { Integer          }
  | bool                           { Bool             }
  | lam                            { Lam              }

{
parseError :: [Token] -> a
parseError tokenList = error ("Grammar Error")

data E = Int Int           |
         Var String        |
         ToyTrue           |
         ToyFalse          |
         Compare E E       |
         Plus E E          |
         If E E E          |
         Lambda String T E |
         App E E           |
         Let String T E E  
         deriving (Show, Eq)

data T = Integer | Bool | Lam
         deriving (Show, Eq)
}