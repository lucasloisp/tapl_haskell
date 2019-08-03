module Lambda.Core (termShift) where

import Lambda.Syntax

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
  where walk c (TmVar x) = TmVar (if x < c then x else x+d)
        walk c (TmAbs t1) = TmAbs (walk (c+1) t1)
        walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)
