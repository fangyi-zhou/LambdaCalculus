module Reduce where
import Term
import Data.List ((\\), union)

reduce :: Term -> Term
reduce t
  | reducible t = reduce $ reduceOne t
  | otherwise   = t

reducible :: Term -> Bool
reducible (App (Abs v t1) t2)
  = True
reducible (App t1 t2)
  = reducible t1 || reducible t2
reducible (Abs v t)
  = reducible t
reducible t
  = False

reduceOne :: Term -> Term
reduceOne (App (Abs v t1) t2)
  | reducible t2 = App (Abs v t1) (reduceOne t2)
  | otherwise    = replace t1 v t2
reduceOne (App t1 t2)
  | reducible t1 = App (reduceOne t1) t2
  | reducible t2 = App t1 (reduceOne t2)
reduceOne (Abs v t)
  | reducible t = Abs v (reduceOne t)

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
