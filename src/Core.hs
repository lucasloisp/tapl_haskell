module Core (eval1) where

import Syntax



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
