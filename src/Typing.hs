module Typing where

import Data.Maybe (fromMaybe)

data Type
  = Base Int
  | FunApp Type Type
  deriving Eq

instance Show Type where
  show (Base i) = "T" ++ show i
  show (FunApp t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

type Var
  = String
type TypingContext
  = [(Var, Type)]
type Substitution
  = (Int, Type)

substitute :: Type -> Substitution -> Type
substitute ty@(Base base) (origin, sub)
  | base == origin = sub
  | otherwise      = ty
substitute (FunApp t1 t2) subs
  = FunApp (substitute t1 subs) (substitute t2 subs)

connectSub :: Substitution -> Substitution -> Substitution
connectSub (b1, t1) (b2, t2)
  = undefined

unify :: Type -> Type -> Maybe Substitution
unify (Base t1) (Base t2)
  = Just (t1, Base t2)
unify (Base t1) ty
  | contains ty t1 = Nothing
  | otherwise      = Just (t1, ty)
  where
    contains :: Type -> Int -> Bool
    contains (Base t1) base
       = t1 == base
    contains (FunApp t1 t2) base
      = contains t1 base || contains t2 base
unify (FunApp t1 t2) (FunApp t3 t4)
  = do
    sub1 <- unify t1 t3
    sub2 <- unify (substitute t2 sub1) (substitute t4 sub1)
    return (connectSub sub2 sub1)
