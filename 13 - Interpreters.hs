--making a simple grammar:
-- =x is a variable
-- \x is a lambda expression, 
-- E E is a function applied to an expression
E :: =x | \x -> E | E E

--applications associate to the left
a b c == (a b) c
--abstraction associates to the right
\x -> \y -> x y == \x -> (\y -> x y)

--(e1,e2) is encoded as \v -> e1 e2
--fst e   is encoded as e (\x -> \y -> x)
--snd e   is encoded as e (\x -> \y -> y)

--x is bound in the expression \x -> y
--the scope of x is y; x is contained in y
--a variable which is not bound anywhere is 'free'
--a lambda with no free variables is closed

--interpreters used beta reduction