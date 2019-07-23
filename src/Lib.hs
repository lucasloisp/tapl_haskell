module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Info = ()
dummyinfo :: Info
dymmyinfo = ()

data Term = TmTrue Info
          | TmFalse Info
          | TmIf Info Term Term Term
          | TmZero Info
          | TmSucc Info Term
          | TmPred Info Term
          | TmIsZero Info Term

isNumerical :: Term -> Bool
isNumerical (TmZero _) = True
isNumerical (TmSucc _ t1) = isNumerical t1
isNumerical _ = False

isVal :: Term -> Bool
isVal (TmTrue _) = True
isVal (TmFalse _) = True
isVal t = isNumerical t

eval1 :: Term -> Maybe Term
eval1 (TmIf _ (TmTrue _) t2 _) = Just t2
eval1 (TmIf _ (TmFalse _) _ t3) = Just t3
eval1 (TmIf fi t1 t2 t3) = do
  t1' <- eval1 t1
  return (TmIf fi t1' t2 t3)
eval1 (TmSucc fi t1) = do
  t1' <- eval1 t1
  return (TmSucc fi t1')
eval1 (TmPred _ (TmZero _)) = TmZero(dummyinfo)
// TODO:


