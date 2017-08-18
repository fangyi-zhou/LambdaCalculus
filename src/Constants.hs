module Constants where

import Term

true :: Term
true = Abs "t" (Abs "f" (Var "t"))

false :: Term
false = Abs "t" (Abs "f" (Var "f"))

and :: Term
and = Abs "b" (Abs "c" (App (App (Var "b") (Var "c")) false))

zero :: Term
zero = Abs "f" (Abs "x" (Var "x"))

one :: Term
one = Abs "f" (Abs "x" (App (Var "f") (Var "x")))

two :: Term
two = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x"))))

succ :: Term
succ = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))

