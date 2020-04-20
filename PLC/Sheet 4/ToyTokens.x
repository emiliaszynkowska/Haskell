-- preamble -----------------------------------------------------------------------

{
module ToyTokens where 
}

-- wrapper ------------------------------------------------------------------------

%wrapper "basic" 

-- macros -------------------------------------------------------------------------

$digit = 0-9    
$alpha = [a-zA-Z]    

-- delimiter ----------------------------------------------------------------------

tokens :-

-- rules --------------------------------------------------------------------------

$white+       ; 
  "--".*      ; 
  let         { \s -> TokenLet          } 
  in          { \s -> TokenIn           }
  if          { \s -> TokenIf           }
  then        { \s -> TokenThen         }
  else        { \s -> TokenElse         }
  integer     { \s -> TokenInteger      }
  bool        { \s -> TokenBool         }
  lam         { \s -> TokenLam          }
  true        { \s -> TokenToyTrue      }
  false       { \s -> TokenToyFalse     }
  $digit+     { \s -> TokenInt (read s) } 
  app         { \s -> TokenApp          }
  \=          { \s -> TokenEq           }
  \+          { \s -> TokenPlus         }
  \<          { \s -> TokenCompare      }
  \(          { \s -> TokenLParen       }
  \)          { \s -> TokenRParen       }
  \:          { \s -> TokenCons         }
  \\          { \s -> TokenLambda       }
  \->         { \s -> TokenArrow        }
  $alpha [$alpha $digit \_ \’]*   { \s -> TokenVar s } 

-- postamble -----------------------------------------------------------------------

{
data Token =
  TokenLet        |
  TokenIn         |
  TokenIf         |
  TokenThen       |
  TokenElse       |
  TokenInteger    |
  TokenBool       |
  TokenLam        |
  TokenToyTrue    |
  TokenToyFalse   |
  TokenInt Int    |
  TokenVar String | 
  TokenApp        |
  TokenEq         |
  TokenPlus       |
  TokenCompare    |
  TokenLParen     |
  TokenRParen     |
  TokenCons       |
  TokenLambda     |
  TokenArrow
  deriving (Show, Eq) 
}