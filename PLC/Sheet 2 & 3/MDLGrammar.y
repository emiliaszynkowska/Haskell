{
module MDLGrammar where
import MDLTokens
}

%name parseMDL
%tokentype { Token }
%error { parseError }

%token
	int          { TokenInt $$ }
	moveforward  { TokenMoveForward }
	movebackward { TokenMoveBackward }
	moveright    { TokenMoveRight }
	moveleft     { TokenMoveLeft }
	rotate       { TokenRotate }

%%
Exp : moveforward X  { MoveForward $2  }
    | movebackward X { MoveBackward $2 }
    | moveright X    { MoveRight $2    }
    | moveleft X	 { MoveLeft $2     }
    | rotate X	     { Rotate $2       }
X   : int            { Int $1          }

{
parseError :: [Token] -> a
parseError tokenList = error ("Parse error")

data Exp =
	MoveForward X  | 
	MoveBackward X | 
	MoveRight X    | 
	MoveLeft X     | 
	Rotate X     
	deriving Show
data X =
	Int Int
	deriving Show
}