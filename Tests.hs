import Data.Char
import Parsing
import Challenges

tests :: [[Bool]]
tests = 
    [ 
      [ -- Challenge 1
      -- Simple tests
      alphaNorm (LamVar 1)                       == (LamVar 1),
      alphaNorm (LamAbs 1 (LamVar 1))            == (LamAbs 0 (LamVar 1)),
      alphaNorm (LamAbs 2 (LamVar 0))            == (LamAbs 0 (LamVar 0)),
      alphaNorm (LamAbs 0 (LamVar 2))            == (LamAbs 0 (LamVar 2)),
      alphaNorm (LamApp (LamVar 1) (LamVar 2))    == (LamApp (LamVar 1) (LamVar 2)),
      -- LamAbs tests
      alphaNorm (LamAbs 0 (LamAbs 0 (LamVar 1))) == (LamAbs 0 (LamAbs 0 (LamVar 1))),
      alphaNorm (LamAbs 1 (LamAbs 1 (LamVar 0))) == (LamAbs 0 (LamAbs 0 (LamVar 0))),
      alphaNorm (LamAbs 1 (LamAbs 1 (LamVar 1))) == (LamAbs 0 (LamAbs 0 (LamVar 0))),
      alphaNorm (LamAbs 0 (LamAbs 1 (LamVar 1))) == (LamAbs 0 (LamAbs 1 (LamVar 1))),
      -- LamApp tests
      alphaNorm (LamApp (LamAbs 1 (LamVar 0)) (LamVar 1))            == (LamApp (LamAbs 0 (LamVar 0)) (LamVar 1)),
      alphaNorm (LamApp (LamVar 1) (LamAbs 0 (LamVar 1)))            == (LamApp (LamVar 1) (LamAbs 0 (LamVar 0))),
      alphaNorm (LamApp (LamAbs 2 (LamAbs 1 (LamVar 0))) (LamVar 0)) == (LamApp (LamAbs 0 (LamAbs 0 (LamVar 0))) (LamVar 0)),
      alphaNorm (LamApp (LamAbs 0 (LamVar 1)) (LamAbs 1 (LamVar 0))) == LamApp (LamAbs 0 (LamVar 0)) (LamAbs 0 (LamVar 0)),
      alphaNorm (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))) == LamApp (LamAbs 0 (LamVar 0)) (LamAbs 0 (LamVar 0)),
      alphaNorm (LamApp (LamVar 1) (LamAbs 2 (LamAbs 2 (LamVar 1)))) == (LamApp (LamVar 1) (LamAbs 0 (LamAbs 0 (LamVar 1))))
      ],    

      [ -- Challenge 2
      -- Tests using different reductions and limits
      countAllReds (LamAbs 0 (LamVar 0)) 0                                == 0, -- Where the limit is 0
      countAllReds (LamVar 0) 1                                           == 1, -- Where the expression cannot be reduced further
      countAllReds (LamVar 1) 1                                           == 1, -- where the expression cannot be reduced further
      countAllReds (LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 0))) (LamAbs 0 (LamApp (LamVar 0) (LamVar 0)))) 3
                                                                          == 1, -- Where the expression is a Y combinator
      countAllReds (LamApp (LamApp (LamAbs 1 (LamAbs 1 (LamVar 1))) (LamVar 0)) (LamApp (LamAbs 2 (LamVar 1)) (LamVar 0))) 2
                                                                          == 0, -- Where there are 0 reductions up to the limit 2
      countAllReds (LamAbs 3 (LamAbs 2 (LamVar 2))) 2                     == 1, -- Where there is 1 reduction up to the limit 2
      countAllReds (LamApp (LamAbs 0 (LamVar 1)) (LamVar 0)) 3            == 1, -- Where there is 1 reduction up to the limit 3
      countAllReds (LamApp (LamAbs 3 (LamVar 2)) (LamAbs 0 (LamVar 0))) 3 == 1, -- Where there is 1 reduction up to the limit 3
      countAllReds (LamApp (LamApp (LamAbs 3 (LamAbs 2 (LamVar 1))) (LamVar 3)) (LamApp (LamAbs 0 (LamVar 1)) (LamVar 2))) 3                   
                                                                          == 1  -- Where there is 1 reduction up to the limit 3
      ],

      [ -- Challenge 3
      -- LamVar and LamApp Tests
      printLambda (LamVar 0) == "x0",
      printLambda (LamApp (LamVar 1) (LamVar 2)) == "x1 x2",
      printLambda (LamApp (LamAbs 0 (LamVar 2)) (LamVar 0)) == "(\\x0 -> x2) x0",
      printLambda (LamApp (LamVar 0) (LamAbs 2 (LamVar 0))) == "x0 (\\x2 -> x0)",
      printLambda (LamApp (LamAbs 0 (LamVar 0)) (LamAbs 1 (LamVar 1))) == "(\\x0 -> x0) \\x1 -> x1",
      -- Recognising Scott Encodings
      printLambda (LamAbs 0 (LamAbs 1 (LamVar 2))) == "0",
      printLambda (LamAbs 0 (LamAbs 1 (LamApp (LamVar 2) (LamAbs 3 (LamAbs 4 (LamVar 5)))))) == "1",
      printLambda (LamAbs 0 (LamAbs 1 (LamApp (LamVar 2) (LamAbs 3 (LamAbs 4 (LamApp (LamVar 5) (LamAbs 6 (LamAbs 7 (LamVar 8))))))))) == "2",
      printLambda (LamAbs 0 (LamAbs 1 (LamApp (LamVar 2) (LamAbs 3 (LamAbs 4 (LamApp (LamVar 5) (LamAbs 6 (LamAbs 7 (LamApp (LamVar 8) (LamAbs 9 (LamAbs 9 (LamVar 9)))))))))))) == "3"
      ],

      [ -- Challenge 4
      -- Nothing tests
      parseLet "let x0 = x0"             == Nothing,
      parseLet "let f0 = f0"             == Nothing,
      parseLet "let x0 x1 = f0"          == Nothing,
      -- LetVar tests
      parseLet "x0"                      == Just (LetVar 0),
      -- LetApp tests
      parseLet "x0 x1 x2"                == Just (LetApp (LetApp (LetVar 0) (LetVar 1)) (LetVar 2)),
      parseLet "(x0 x1) x2"              == Just (LetApp (LetApp (LetVar 0) (LetVar 1)) (LetVar 2)),
      parseLet "x0 (x1 x2)"              == Just (LetApp (LetVar 0) (LetApp (LetVar 1) (LetVar 2))),
      -- LetDef tests
      parseLet "let f0 x0 = x0 in f0"    == Just (LetDef [([0,0],LetVar 0)] (LetFun 0)),
      parseLet "let f0 x1 = x2 in f0"    == Just (LetDef [([0,1],LetVar 2)] (LetFun 0)),
      parseLet "let f0 x0 = f1 in f1"    == Just (LetDef [([0,0],LetFun 1)] (LetFun 1)),
      -- LetDef tests with longer equation lists
      parseLet "let f0 x0 x1 = x2 in f0" == Just (LetDef [([0,0,1],LetVar 2)] (LetFun 0)),
      parseLet "let f1 x1 x2 = x3 in f1" == Just (LetDef [([1,1,2],LetVar 3)] (LetFun 1)),
      -- LetDef tests with multiple equations and equation lists
      parseLet "let f0 x2 = x3; f1 x2 = x1 in f1"
                                         == Just (LetDef [([0,2],LetVar 3),([1,2],LetVar 1)] (LetFun 1)),
      parseLet "let f0 x0 = x1; f1 x1 = x2 in f2 x1" 
                                         == Just (LetDef [([0,0],LetVar 1),([1,1],LetVar 2)] (LetApp (LetFun 2) (LetVar 1))),
      parseLet "let f0 x1 x2 = x2; f1 x1 x2 = x3 in f1 x1 x2"  
                                         == Just (LetDef [([0,1,2],LetVar 2),([1,1,2],LetVar 3)] (LetApp (LetFun 1) (LetApp (LetVar 1) (LetVar 2))))                                
      ],

      [ -- Challenge 5
      -- LetVar tests
      letToLambda (LetVar 0) == (LamVar 0),                                                                                                                                                                     
      -- LetDef tests
      letToLambda (LetDef [([0,0],LetVar 1)] (LetFun 0))      == (LamAbs 0 (LamVar 1)),
      letToLambda (LetDef [([0,0,1],LetVar 2)] (LetFun 0))    == (LamAbs 0 (LamAbs 1 (LamVar 2))),
      letToLambda (LetDef [([0,0,1,2],LetVar 3)] (LetFun 0))  == (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 3)))),
      -- LetApp tests
      letToLambda (LetApp (LetVar 1) (LetVar 2))              == (LamApp (LamVar 1) (LamVar 2)),
      letToLambda (LetDef [([0,0],LetVar 1)] (LetApp (LetFun 0) (LetVar 0))) 
                                                              == (LamApp (LamAbs 0 (LamVar 1)) (LamVar 0)),
      letToLambda (LetDef [([0,0],LetVar 1)] (LetApp (LetVar 0) (LetFun 0)))
                                                              == (LamApp (LamVar 0) (LamAbs 0 (LamVar 1))),
      letToLambda (LetDef [([0,0],LetVar 1), ([1,2],LetVar 3)] (LetApp (LetFun 0) (LetFun 1)))
                                                              == (LamApp (LamAbs 0 (LamVar 1)) (LamAbs 2 (LamVar 3))),   
      letToLambda (LetDef [([0,0,1],LetVar 2)] (LetApp (LetFun 0) (LetVar 0)))
                                                              == (LamApp (LamAbs 0 (LamAbs 1 (LamVar 2))) (LamVar 0))
      ],

      [ -- Challenge 6
      -- LamVar tests
      lambdaToLet (LamVar 0)                                  == (LetVar 0),
      -- LamAbs tests
      lambdaToLet (LamAbs 0 (LamVar 1))                       == (LetDef [([0,0],LetVar 1)] (LetFun 0)),               
      lambdaToLet (LamAbs 0 (LamAbs 1 (LamVar 2)))            == (LetDef [([0,0,1],LetVar 2)] (LetFun 0)),
      lambdaToLet (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 3)))) == (LetDef [([0,0,1,2],LetVar 3)] (LetFun 0)),
      -- LamApp tests
      lambdaToLet (LamApp (LamVar 1) (LamVar 2))              == (LetApp (LetVar 1) (LetVar 2)),
      lambdaToLet (LamApp (LamAbs 0 (LamVar 1)) (LamVar 0))   == (LetDef [([0,0],LetVar 1)] (LetApp (LetFun 0) (LetVar 0))),
      lambdaToLet (LamApp (LamVar 0) (LamAbs 0 (LamVar 1)))   == (LetDef [([0,0],LetVar 1)] (LetApp (LetVar 0) (LetFun 0))),
      lambdaToLet (LamApp (LamAbs 0 (LamVar 1)) (LamAbs 2 (LamVar 3))) 
                                                              == (LetDef [([0,0],LetVar 1), ([1,2],LetVar 3)] (LetApp (LetFun 0) (LetFun 1))),
      lambdaToLet (LamApp (LamAbs 0 (LamAbs 1 (LamVar 2))) (LamVar 0)) 
                                                              == (LetDef [([0,0,1],LetVar 2)] (LetApp (LetFun 0) (LetVar 0)))                                                       
      ]
    ]

main :: IO ()
main = 
  do
    putStrLn "... Testing ..."
    testSuite tests 
    putStrLn "... Completed ..."

-- process one test suite at a time
testSuite :: [[Bool]] -> IO ()
testSuite [] = 
  do
    putStr ""
testSuite (bs : bbs) =
  do
    putStrLn ("  " ++ show (length [b | b <- bs, b]) ++ " tests passed out of " ++ show (length bs))
    testSuite bbs