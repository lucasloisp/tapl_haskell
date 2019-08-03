module Lambda.Syntax where

type Index = Int

data Term = TmVar Index
          | TmAbs Term
          | TmApp Term Term
          deriving (Show, Eq)
