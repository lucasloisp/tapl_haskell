module Lambda.Core (termShift, termSubst, eval, eval1) where

import Lambda.Syntax

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
  where walk c (TmVar x) = TmVar (if x < c then x else x+d)
        walk c (TmAbs t1) = TmAbs (walk (c+1) t1)
        walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walk 0 t
  where walk c (TmVar x) = if x == j+c then termShift c s else (TmVar x)
        walk c (TmAbs t1) = TmAbs (walk (c+1) t1)
        walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

termSubstTop s t = termShift (negate 1) (termSubst 0 (termShift 1 s) t)

isVal :: Term -> Bool
isVal (TmAbs _) = True
isVal _ = False

eval1 :: Term -> Maybe Term
eval1 (TmApp (TmAbs t12) v2) | isVal v2 = return $ termSubstTop v2 t12
eval1 (TmApp v1 t2) | isVal v1 = do
  t2' <- eval1 t2
  return $ TmApp v1 t2'
eval1 (TmApp t1 t2) = do
  t1' <- eval1 t1
  return $ TmApp t1' t2
eval1 _ = Nothing

eval :: Term -> Term
eval t = case eval1 t of Nothing -> t
                         Just t' -> eval t'
