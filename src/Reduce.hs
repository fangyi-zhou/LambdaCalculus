module Reduce where
import Term
import Data.List ((\\), union)

reduce :: Term -> Term
reduce (Abs var term)
  = Abs var (reduce term)
reduce (App (Abs var t1) t2)
  = reduce (replace t1 var t2)
reduce t
  = t

reducible :: Term -> Bool
reducible (App (Abs v t1) t2)
  = True
reducible (App t1 t2)
  = reducible t1 || reducible t2
reducible t
  = False

replace :: Term -> String -> Term -> Term
replace (Var v1) v2 t
  | v1 == v2  = t
  | otherwise = Var v1
replace (Abs v1 term) v2 t
  | (v1 /= v2) && notElem v2 (freeVar t) = Abs v1 (replace term v2 t)
  | otherwise = Abs v1 term
replace (App t1 t2) v2 t
  = App (replace t1 v2 t) (replace t2 v2 t)

freeVar :: Term -> [String]
freeVar (Var v) = [v]
freeVar (Abs var term) = freeVar term \\ [var]
freeVar (App t1 t2) = freeVar t1 `union` freeVar t2
