-- preamble -----------------------------------------------------------------------
-- MDL = Maze Direction Language

{ 
module MDLTokens where 
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
  $digit+       { \s -> TokenInt (read s) } 
  moveforward   { \s -> TokenMoveForward }
  movebackward  { \s -> TokenMoveBackward }
  moveright     { \s -> TokenMoveRight }
  moveleft      { \s -> TokenMoveLeft }
  rotate        { \s -> TokenRotate }

-- postamble -----------------------------------------------------------------------

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenInt Int      |
  TokenMoveForward  |
  TokenMoveBackward |
  TokenMoveRight    |
  TokenMoveLeft     |
  TokenRotate       
  deriving Show
}