-- preamble -----------------------------------------------------------------------

{
module Tokens where 
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
  "--".*        ; 
  let           { \s -> TokenLet } 
  in            { \s -> TokenIn }
  $digit+       { \s -> TokenInt (read s) } 
  \=          { \s -> TokenEq }
  \+          { \s -> TokenPlus }
  \-          { \s -> TokenMinus }
  \*          { \s -> TokenTimes }
  \^          { \s -> TokenPow }
  \/          { \s -> TokenDiv }
  \(          { \s -> TokenLParen }
  \)          { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \’]*   { \s -> TokenVar s } 

-- postamble -----------------------------------------------------------------------

{ 
data Token = 
  TokenLet        | 
  TokenIn         | 
  TokenInt Int    |
  TokenVar String | 
  TokenEq         |
  TokenPlus       |
  TokenMinus      |
  TokenTimes      |
  TokenPow        |
  TokenDiv        |
  TokenLParen     |
  TokenRParen      
  deriving (Eq,Show) 
}