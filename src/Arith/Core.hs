module Arith.Core (eval1, eval, evalBig) where

import Arith.Syntax



isNumerical :: Term -> Bool
isNumerical (TmZero _) = True
isNumerical (TmSucc _ t1) = isNumerical t1
isNumerical _ = False

isVal :: Term -> Bool
isVal (TmTrue _) = True
isVal (TmFalse _) = True
isVal t = isNumerical t

eval1 :: Term -> Maybe Term
eval1 (TmIf _ (TmTrue _) t2 _) = return t2
eval1 (TmIf _ (TmFalse _) _ t3) = return t3
eval1 (TmIf fi t1 t2 t3) = do
  t1' <- eval1 t1
  return (TmIf fi t1' t2 t3)
eval1 (TmSucc fi t1) = do
  t1' <- eval1 t1
  return (TmSucc fi t1')
eval1 (TmPred _ (TmZero _)) = return $ TmZero dummyinfo
eval1 (TmPred _ (TmSucc _ nv1))
  | isNumerical nv1 = return nv1
  | otherwise       = Nothing
eval1 (TmPred f1 t1) = do
  t1' <- eval1 t1
  return (TmPred f1 t1')
eval1 (TmIsZero _ (TmZero _)) = return $ TmTrue dummyinfo
eval1 (TmIsZero _ (TmSucc _ nv1))
  | isNumerical nv1 = return $ TmFalse dummyinfo
  | otherwise = Nothing
eval1 (TmIsZero fi t1) = do
  t1' <- eval1 t1
  return $ TmIsZero fi t1'
eval1 _ = Nothing

eval :: Term -> Term
eval t = case eval1 t of Nothing -> t
                         Just t' -> eval t'
evalBig :: Term -> Term
evalBig t@(TmIsZero _ t1) = case evalBig t1 of (TmZero _)    -> (TmTrue ())
                                               (TmSucc _ _)  -> (TmFalse ())
                                               _             -> t
evalBig t@(TmPred _ t1) = case evalBig t1 of (TmZero _)      -> (TmZero ())
                                             (TmSucc _ nv)   -> nv
					     _               -> t
evalBig t@(TmSucc _ t1) = case evalBig t1 of (TmZero _)      -> (TmSucc () (TmZero ()))
                                             (TmSucc _ nv)   -> (TmSucc () nv)
					     _               -> t
evalBig t@(TmIf _ t1 t2 t3) = case evalBig t1 of (TmTrue _)  -> t2'
                                                 (TmFalse _) -> t3'
						 _           -> t
			      where t2' = evalBig t2
			            t3' = evalBig t3
evalBig t = t
