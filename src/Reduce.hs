module Reduce where
import Term

reduce :: Term -> Term
reduce (Abs var term)
  = Abs var (reduce term)
reduce (App (Abs var t1) t2)
  = reduce (replace t1 var t2)
reduce t
  = t

replace :: Term -> String -> Term -> Term
replace (Var v1) v2 t
  | v1 == v2  = t
  | otherwise = (Var v1)
replace (Abs v1 term) v2 t
  | v1 == v2  = (Abs v1 term)
  | otherwise = Abs v1 (replace term v2 t)
replace (App t1 t2) v2 t
  = App (replace t1 v2 t) (replace t2 v2 t)
