-- preamble -----------------------------------------------------------------------

{
module Tokens2 where 
}

-- wrapper ------------------------------------------------------------------------

%wrapper "posn" 

-- macros -------------------------------------------------------------------------

$digit = 0-9    
$alpha = [a-zA-Z]    

-- delimiter ----------------------------------------------------------------------

tokens :-

-- rules --------------------------------------------------------------------------

$white+       ; 
  "--".*        ; 
  let           { \p s -> TokenLet p } 
  in            { \p s -> TokenIn p }
  $digit+       { \p s -> TokenInt p (read s) } 
  \=          { \p s -> TokenEq p }
  \+          { \p s -> TokenPlus p }
  \-          { \p s -> TokenMinus p }
  \*          { \p s -> TokenTimes p }
  \^          { \p s -> TokenPow p }
  \/          { \p s -> TokenDiv p }
  \(          { \p s -> TokenLParen p }
  \)          { \p s -> TokenRParen p }
  $alpha [$alpha $digit \_ \’]*   { \p s -> TokenVar p s } 

-- postamble -----------------------------------------------------------------------

{ 
tokenPosn :: Token -> AlexPosn
tokenPosn (TokenLet p)    = p
tokenPosn (TokenIn p)     = p
tokenPosn (TokenInt p _)  = p
tokenPosn (TokenVar p _)  = p
tokenPosn (TokenEq p)     = p
tokenPosn (TokenPlus p)   = p
tokenPosn (TokenMinus p)  = p
tokenPosn (TokenTimes p)  = p
tokenPosn (TokenPow p)    = p
tokenPosn (TokenDiv p)    = p
tokenPosn (TokenLParen p) = p
tokenPosn (TokenRParen p) = p

showPosn :: Token -> String
showPosn (TokenLet    (AlexPn a l c)) = "line " ++ show(l) ++ " column " ++ show(c)
showPosn (TokenIn     (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
showPosn (TokenInt    (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
showPosn (TokenVar    (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
showPosn (TokenEq     (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
showPosn (TokenPlus   (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
showPosn (TokenMinus  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
showPosn (TokenTimes  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
showPosn (TokenPow    (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
showPosn (TokenDiv    (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
showPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
showPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

data Token = 
  TokenLet AlexPosn        | 
  TokenIn AlexPosn         | 
  TokenInt AlexPosn Int    |
  TokenVar AlexPosn String | 
  TokenEq AlexPosn         |
  TokenPlus AlexPosn       |
  TokenMinus AlexPosn      |
  TokenTimes AlexPosn      |
  TokenPow AlexPosn        |
  TokenDiv AlexPosn        |
  TokenLParen AlexPosn     |
  TokenRParen AlexPosn      
  deriving (Eq,Show) 
}

