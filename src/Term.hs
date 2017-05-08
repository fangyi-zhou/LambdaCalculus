module Term where

data Term
  = Var String
  | Abs String Term
  | App Term Term

instance Show Term where
  show (Var v)
    = v
  show (Abs v t)
    = "\\" ++ v ++ ". " ++ show t
  show (App t1 t2)
    = "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"
