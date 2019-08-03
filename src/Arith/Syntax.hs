module Arith.Syntax where

type Info = ()
dummyinfo :: Info
dummyinfo = ()

data Term = TmTrue Info
          | TmFalse Info
          | TmIf Info Term Term Term
          | TmZero Info
          | TmSucc Info Term
          | TmPred Info Term
          | TmIsZero Info Term
          deriving (Show, Eq)
