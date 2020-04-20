{-# OPTIONS_GHC -w #-}
module ToyGrammar where
import ToyTokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,84) ([57504,259,1024,0,0,0,48,0,20482,33264,0,0,0,0,0,0,256,0,8,512,2560,4158,128,12,1024,5120,8316,57504,259,0,0,1024,0,256,15882,16,11264,0,512,896,0,8192,4096,192,1792,0,32768,0,0,0,0,0,5120,8316,57504,259,0,2,32768,0,176,0,0,512,33408,1039,0,40960,992,1,192,16,16390,1985,2,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseToy","E","T","let","in","if","then","else","integer","bool","lam","true","false","int","var","app","'='","'+'","'<'","'('","')'","':'","'\\\\'","'->'","%eof"]
        bit_start = st * 27
        bit_end = (st + 1) * 27
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..26]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (6) = happyShift action_4
action_0 (8) = happyShift action_5
action_0 (14) = happyShift action_6
action_0 (15) = happyShift action_7
action_0 (16) = happyShift action_2
action_0 (17) = happyShift action_8
action_0 (18) = happyShift action_9
action_0 (25) = happyShift action_10
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (16) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (20) = happyShift action_15
action_3 (21) = happyShift action_16
action_3 (27) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (22) = happyShift action_14
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (6) = happyShift action_4
action_5 (8) = happyShift action_5
action_5 (14) = happyShift action_6
action_5 (15) = happyShift action_7
action_5 (16) = happyShift action_2
action_5 (17) = happyShift action_8
action_5 (18) = happyShift action_9
action_5 (25) = happyShift action_10
action_5 (4) = happyGoto action_13
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_3

action_7 _ = happyReduce_4

action_8 _ = happyReduce_2

action_9 (22) = happyShift action_12
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (22) = happyShift action_11
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (17) = happyShift action_22
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (6) = happyShift action_4
action_12 (8) = happyShift action_5
action_12 (14) = happyShift action_6
action_12 (15) = happyShift action_7
action_12 (16) = happyShift action_2
action_12 (17) = happyShift action_8
action_12 (18) = happyShift action_9
action_12 (25) = happyShift action_10
action_12 (4) = happyGoto action_21
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (9) = happyShift action_20
action_13 (20) = happyShift action_15
action_13 (21) = happyShift action_16
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (17) = happyShift action_19
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (6) = happyShift action_4
action_15 (8) = happyShift action_5
action_15 (14) = happyShift action_6
action_15 (15) = happyShift action_7
action_15 (16) = happyShift action_2
action_15 (17) = happyShift action_8
action_15 (18) = happyShift action_9
action_15 (25) = happyShift action_10
action_15 (4) = happyGoto action_18
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (6) = happyShift action_4
action_16 (8) = happyShift action_5
action_16 (14) = happyShift action_6
action_16 (15) = happyShift action_7
action_16 (16) = happyShift action_2
action_16 (17) = happyShift action_8
action_16 (18) = happyShift action_9
action_16 (25) = happyShift action_10
action_16 (4) = happyGoto action_17
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (21) = happyFail []
action_17 _ = happyReduce_5

action_18 (21) = happyShift action_16
action_18 _ = happyReduce_6

action_19 (24) = happyShift action_26
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (6) = happyShift action_4
action_20 (8) = happyShift action_5
action_20 (14) = happyShift action_6
action_20 (15) = happyShift action_7
action_20 (16) = happyShift action_2
action_20 (17) = happyShift action_8
action_20 (18) = happyShift action_9
action_20 (25) = happyShift action_10
action_20 (4) = happyGoto action_25
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (20) = happyShift action_15
action_21 (21) = happyShift action_16
action_21 (23) = happyShift action_24
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (24) = happyShift action_23
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (11) = happyShift action_28
action_23 (12) = happyShift action_29
action_23 (13) = happyShift action_30
action_23 (5) = happyGoto action_33
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (22) = happyShift action_32
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (10) = happyShift action_31
action_25 (20) = happyShift action_15
action_25 (21) = happyShift action_16
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (11) = happyShift action_28
action_26 (12) = happyShift action_29
action_26 (13) = happyShift action_30
action_26 (5) = happyGoto action_27
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (23) = happyShift action_37
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_11

action_29 _ = happyReduce_12

action_30 _ = happyReduce_13

action_31 (6) = happyShift action_4
action_31 (8) = happyShift action_5
action_31 (14) = happyShift action_6
action_31 (15) = happyShift action_7
action_31 (16) = happyShift action_2
action_31 (17) = happyShift action_8
action_31 (18) = happyShift action_9
action_31 (25) = happyShift action_10
action_31 (4) = happyGoto action_36
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (6) = happyShift action_4
action_32 (8) = happyShift action_5
action_32 (14) = happyShift action_6
action_32 (15) = happyShift action_7
action_32 (16) = happyShift action_2
action_32 (17) = happyShift action_8
action_32 (18) = happyShift action_9
action_32 (25) = happyShift action_10
action_32 (4) = happyGoto action_35
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (23) = happyShift action_34
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (26) = happyShift action_40
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (20) = happyShift action_15
action_35 (21) = happyShift action_16
action_35 (23) = happyShift action_39
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (21) = happyFail []
action_36 _ = happyReduce_7

action_37 (19) = happyShift action_38
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (6) = happyShift action_4
action_38 (8) = happyShift action_5
action_38 (14) = happyShift action_6
action_38 (15) = happyShift action_7
action_38 (16) = happyShift action_2
action_38 (17) = happyShift action_8
action_38 (18) = happyShift action_9
action_38 (25) = happyShift action_10
action_38 (4) = happyGoto action_42
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_9

action_40 (6) = happyShift action_4
action_40 (8) = happyShift action_5
action_40 (14) = happyShift action_6
action_40 (15) = happyShift action_7
action_40 (16) = happyShift action_2
action_40 (17) = happyShift action_8
action_40 (18) = happyShift action_9
action_40 (25) = happyShift action_10
action_40 (4) = happyGoto action_41
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (20) = happyShift action_15
action_41 (21) = happyShift action_16
action_41 _ = happyReduce_8

action_42 (7) = happyShift action_43
action_42 (20) = happyShift action_15
action_42 (21) = happyShift action_16
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (6) = happyShift action_4
action_43 (8) = happyShift action_5
action_43 (14) = happyShift action_6
action_43 (15) = happyShift action_7
action_43 (16) = happyShift action_2
action_43 (17) = happyShift action_8
action_43 (18) = happyShift action_9
action_43 (25) = happyShift action_10
action_43 (4) = happyGoto action_44
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (21) = happyFail []
action_44 _ = happyReduce_10

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn4
		 (Int happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyTerminal (TokenVar happy_var_1))
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
		 (Plus happy_var_1 happy_var_3
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

happyReduce_8 = happyReduce 8 4 happyReduction_8
happyReduction_8 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lambda happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 7 4 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (App happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 10 4 happyReduction_10
happyReduction_10 ((HappyAbsSyn4  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_3 happy_var_5 happy_var_8 happy_var_10
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  5 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn5
		 (Integer
	)

happyReduce_12 = happySpecReduce_1  5 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn5
		 (Bool
	)

happyReduce_13 = happySpecReduce_1  5 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn5
		 (Lam
	)

happyNewToken action sts stk [] =
	action 27 27 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLet -> cont 6;
	TokenIn -> cont 7;
	TokenIf -> cont 8;
	TokenThen -> cont 9;
	TokenElse -> cont 10;
	TokenInteger -> cont 11;
	TokenBool -> cont 12;
	TokenLam -> cont 13;
	TokenToyTrue -> cont 14;
	TokenToyFalse -> cont 15;
	TokenInt happy_dollar_dollar -> cont 16;
	TokenVar happy_dollar_dollar -> cont 17;
	TokenApp -> cont 18;
	TokenEq -> cont 19;
	TokenPlus -> cont 20;
	TokenCompare -> cont 21;
	TokenLParen -> cont 22;
	TokenRParen -> cont 23;
	TokenCons -> cont 24;
	TokenLambda -> cont 25;
	TokenArrow -> cont 26;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 27 tk tks = happyError' (tks, explist)
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
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parseToy tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
