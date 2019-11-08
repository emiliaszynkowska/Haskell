--reduction: step of a computation
--redex: function applied to an argument, is reducible e.g. not (x == 0), not (True), not (not True), add (1)

--innermost: contains no other redex
--outermost: not contained in any redex

--we use lambda \ notation for functions, e.g. \x -> e. x is bound in the expression e
--beta reduction: to reduce the redex (\x -> e1) e2 we must replace every x in e1 with e2
--e.g. (\x -> 2*x*x + y)(7) => 2*7*7 + y

mult :: Int -> Int -> Int
mult = \x -> \y -> x * y

--call-by-value: innermost reduction with no reduction under lambda, calculates any inner values first
--call-by-name: outermost reduction under lambda, doesn't bother doing inside stuff first before returning the expression

--graph reduction: an efficient way of reduction..
--instead of copying an expression multiple times as part of beta reduction..
--a single copy of the expression is kept and pointers are passed to the function instead

--lazy evaluation: combination of call-by-name and graph reduction

--the operator $! forces strict evaluation order
(f $! x) y -- forces top-level evaluation of x
(f x) $! y -- forces top-level evaluation of y
(f $! x) $! y -- forces top-level evaluation of x and y






