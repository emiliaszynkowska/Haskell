{-# OPTIONS_GHC -w #-}
module ToyGrammar where 
import ToyTokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5
	= HappyTerminal (ToyToken)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,122) ([26368,70,2,0,0,13304,2,0,0,0,0,13184,35,16384,0,128,6556,63489,1587,512,0,4,49088,32785,8755,26368,70,36046,39936,273,9016,28674,1126,0,1,512,0,24576,0,192,0,7676,14337,563,8,8,0,0,16384,16384,96,0,8192,39936,281,0,0,0,52448,8,0,16256,51,18023,52736,136,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","Exp","Type","Bool","Int","arrow","int","true","false","'<'","'+'","var","if","then","else","lam","let","':'","'='","in","'('","')'","%eof"]
        bit_start = st * 25
        bit_end = (st + 1) * 25
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..24]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (9) = happyShift action_2
action_0 (10) = happyShift action_4
action_0 (11) = happyShift action_5
action_0 (14) = happyShift action_6
action_0 (15) = happyShift action_7
action_0 (18) = happyShift action_8
action_0 (19) = happyShift action_9
action_0 (23) = happyShift action_10
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (9) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (9) = happyShift action_2
action_3 (10) = happyShift action_4
action_3 (11) = happyShift action_5
action_3 (12) = happyShift action_16
action_3 (13) = happyShift action_17
action_3 (14) = happyShift action_6
action_3 (15) = happyShift action_7
action_3 (18) = happyShift action_8
action_3 (19) = happyShift action_9
action_3 (23) = happyShift action_10
action_3 (25) = happyAccept
action_3 (4) = happyGoto action_15
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 _ = happyReduce_4

action_6 _ = happyReduce_2

action_7 (9) = happyShift action_2
action_7 (10) = happyShift action_4
action_7 (11) = happyShift action_5
action_7 (14) = happyShift action_6
action_7 (15) = happyShift action_7
action_7 (18) = happyShift action_8
action_7 (19) = happyShift action_9
action_7 (23) = happyShift action_10
action_7 (4) = happyGoto action_14
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (23) = happyShift action_13
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (23) = happyShift action_12
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (9) = happyShift action_2
action_10 (10) = happyShift action_4
action_10 (11) = happyShift action_5
action_10 (14) = happyShift action_6
action_10 (15) = happyShift action_7
action_10 (18) = happyShift action_8
action_10 (19) = happyShift action_9
action_10 (23) = happyShift action_10
action_10 (4) = happyGoto action_11
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (9) = happyShift action_2
action_11 (10) = happyShift action_4
action_11 (11) = happyShift action_5
action_11 (12) = happyShift action_16
action_11 (13) = happyShift action_17
action_11 (14) = happyShift action_6
action_11 (15) = happyShift action_7
action_11 (18) = happyShift action_8
action_11 (19) = happyShift action_9
action_11 (23) = happyShift action_10
action_11 (24) = happyShift action_23
action_11 (4) = happyGoto action_15
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (14) = happyShift action_22
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (14) = happyShift action_21
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (9) = happyShift action_2
action_14 (10) = happyShift action_4
action_14 (11) = happyShift action_5
action_14 (12) = happyShift action_16
action_14 (13) = happyShift action_17
action_14 (14) = happyShift action_6
action_14 (15) = happyShift action_7
action_14 (16) = happyShift action_20
action_14 (18) = happyShift action_8
action_14 (19) = happyShift action_9
action_14 (23) = happyShift action_10
action_14 (4) = happyGoto action_15
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (9) = happyShift action_2
action_15 (10) = happyShift action_4
action_15 (11) = happyShift action_5
action_15 (14) = happyShift action_6
action_15 (15) = happyShift action_7
action_15 (19) = happyShift action_9
action_15 (23) = happyShift action_10
action_15 (4) = happyGoto action_15
action_15 _ = happyReduce_10

action_16 (9) = happyShift action_2
action_16 (10) = happyShift action_4
action_16 (11) = happyShift action_5
action_16 (14) = happyShift action_6
action_16 (15) = happyShift action_7
action_16 (18) = happyShift action_8
action_16 (19) = happyShift action_9
action_16 (23) = happyShift action_10
action_16 (4) = happyGoto action_19
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (9) = happyShift action_2
action_17 (10) = happyShift action_4
action_17 (11) = happyShift action_5
action_17 (14) = happyShift action_6
action_17 (15) = happyShift action_7
action_17 (18) = happyShift action_8
action_17 (19) = happyShift action_9
action_17 (23) = happyShift action_10
action_17 (4) = happyGoto action_18
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (9) = happyShift action_2
action_18 (10) = happyShift action_4
action_18 (11) = happyShift action_5
action_18 (14) = happyShift action_6
action_18 (15) = happyShift action_7
action_18 (19) = happyShift action_9
action_18 (23) = happyShift action_10
action_18 (4) = happyGoto action_15
action_18 _ = happyReduce_6

action_19 (9) = happyShift action_2
action_19 (10) = happyShift action_4
action_19 (11) = happyShift action_5
action_19 (14) = happyShift action_6
action_19 (15) = happyShift action_7
action_19 (19) = happyShift action_9
action_19 (23) = happyShift action_10
action_19 (4) = happyGoto action_15
action_19 _ = happyReduce_5

action_20 (9) = happyShift action_2
action_20 (10) = happyShift action_4
action_20 (11) = happyShift action_5
action_20 (14) = happyShift action_6
action_20 (15) = happyShift action_7
action_20 (18) = happyShift action_8
action_20 (19) = happyShift action_9
action_20 (23) = happyShift action_10
action_20 (4) = happyGoto action_26
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (20) = happyShift action_25
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (20) = happyShift action_24
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_11

action_24 (6) = happyShift action_29
action_24 (7) = happyShift action_30
action_24 (5) = happyGoto action_31
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (6) = happyShift action_29
action_25 (7) = happyShift action_30
action_25 (5) = happyGoto action_28
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (9) = happyShift action_2
action_26 (10) = happyShift action_4
action_26 (11) = happyShift action_5
action_26 (12) = happyShift action_16
action_26 (13) = happyShift action_17
action_26 (14) = happyShift action_6
action_26 (15) = happyShift action_7
action_26 (17) = happyShift action_27
action_26 (18) = happyShift action_8
action_26 (19) = happyShift action_9
action_26 (23) = happyShift action_10
action_26 (4) = happyGoto action_15
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (9) = happyShift action_2
action_27 (10) = happyShift action_4
action_27 (11) = happyShift action_5
action_27 (14) = happyShift action_6
action_27 (15) = happyShift action_7
action_27 (18) = happyShift action_8
action_27 (19) = happyShift action_9
action_27 (23) = happyShift action_10
action_27 (4) = happyGoto action_35
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (8) = happyShift action_32
action_28 (24) = happyShift action_34
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_12

action_30 _ = happyReduce_13

action_31 (8) = happyShift action_32
action_31 (24) = happyShift action_33
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (6) = happyShift action_29
action_32 (7) = happyShift action_30
action_32 (5) = happyGoto action_38
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (21) = happyShift action_37
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (9) = happyShift action_2
action_34 (10) = happyShift action_4
action_34 (11) = happyShift action_5
action_34 (14) = happyShift action_6
action_34 (15) = happyShift action_7
action_34 (18) = happyShift action_8
action_34 (19) = happyShift action_9
action_34 (23) = happyShift action_10
action_34 (4) = happyGoto action_36
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (9) = happyFail []
action_35 (10) = happyFail []
action_35 (11) = happyFail []
action_35 (14) = happyFail []
action_35 (15) = happyFail []
action_35 (23) = happyFail []
action_35 (4) = happyGoto action_15
action_35 _ = happyReduce_7

action_36 (9) = happyFail []
action_36 (10) = happyFail []
action_36 (11) = happyFail []
action_36 (14) = happyFail []
action_36 (15) = happyFail []
action_36 (23) = happyFail []
action_36 (4) = happyGoto action_15
action_36 _ = happyReduce_8

action_37 (9) = happyShift action_2
action_37 (10) = happyShift action_4
action_37 (11) = happyShift action_5
action_37 (14) = happyShift action_6
action_37 (15) = happyShift action_7
action_37 (18) = happyShift action_8
action_37 (19) = happyShift action_9
action_37 (23) = happyShift action_10
action_37 (4) = happyGoto action_39
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_14

action_39 (9) = happyShift action_2
action_39 (10) = happyShift action_4
action_39 (11) = happyShift action_5
action_39 (12) = happyShift action_16
action_39 (13) = happyShift action_17
action_39 (14) = happyShift action_6
action_39 (15) = happyShift action_7
action_39 (18) = happyShift action_8
action_39 (19) = happyShift action_9
action_39 (22) = happyShift action_40
action_39 (23) = happyShift action_10
action_39 (4) = happyGoto action_15
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (9) = happyShift action_2
action_40 (10) = happyShift action_4
action_40 (11) = happyShift action_5
action_40 (14) = happyShift action_6
action_40 (15) = happyShift action_7
action_40 (18) = happyShift action_8
action_40 (19) = happyShift action_9
action_40 (23) = happyShift action_10
action_40 (4) = happyGoto action_41
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (9) = happyShift action_2
action_41 (10) = happyShift action_4
action_41 (11) = happyShift action_5
action_41 (14) = happyShift action_6
action_41 (15) = happyShift action_7
action_41 (19) = happyShift action_9
action_41 (23) = happyShift action_10
action_41 (4) = happyGoto action_15
action_41 _ = happyReduce_9

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenInt _ happy_var_1))
	 =  HappyAbsSyn4
		 (Int happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (ToyTrue
	)

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn4
		 (ToyFalse
	)

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Compare happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Add happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 6 4 happyReduction_7
happyReduction_7 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 7 4 happyReduction_8
happyReduction_8 ((HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lambda happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 10 4 happyReduction_9
happyReduction_9 ((HappyAbsSyn4  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_3 happy_var_5 happy_var_8 happy_var_10
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_2  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App happy_var_1 happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  5 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn5
		 (TypeBool
	)

happyReduce_13 = happySpecReduce_1  5 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn5
		 (TypeInt
	)

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TypeFun happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 25 25 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenTypeBool _ -> cont 6;
	TokenTypeInt _ -> cont 7;
	TokenArrow _ -> cont 8;
	TokenInt _ happy_dollar_dollar -> cont 9;
	TokenToyTrue _ -> cont 10;
	TokenToyFalse _ -> cont 11;
	TokenLessThan _ -> cont 12;
	TokenPlus _ -> cont 13;
	TokenVar _ happy_dollar_dollar -> cont 14;
	TokenIf _ -> cont 15;
	TokenThen _ -> cont 16;
	TokenElse _ -> cont 17;
	TokenLambda _ -> cont 18;
	TokenLet _ -> cont 19;
	TokenCons _ -> cont 20;
	TokenEq _ -> cont 21;
	TokenIn _ -> cont 22;
	TokenLParen _ -> cont 23;
	TokenRParen _ -> cont 24;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 25 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(ToyToken)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parseCalc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "D:/GitHub/haskell-platform/build/ghc-bindist/local/lib/include/ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "F:/Users/randy/AppData/Local/Temp/ghc1900_0/ghc_2.h" #-}


























































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates\\\\GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 75 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 84 "templates\\\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 147 "templates\\\\GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates\\\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates\\\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
