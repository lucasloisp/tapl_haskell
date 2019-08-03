module LambdaSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Test.QuickCheck
import Lambda.Core
import Lambda.Syntax

spec :: Spec
spec =  do
  describe "Lambda.Core.termShift" $ do
    it "is the does not change variables when shifting by 0" $ property $
      \i -> termShift 0 (TmVar i) == (TmVar i)
    it "correctly does not shift a bound variable" $ property $
      \i -> termShift (getPositive i) (TmAbs (TmVar 0)) == (TmAbs (TmVar 0))
    it "shifts a free variable in an abstraction" $ property $
      \d ->
      \f ->
        termShift (getPositive d) (TmAbs (TmVar (getPositive f))) ==
	(TmAbs (TmVar ((getPositive f) + (getPositive d))))
    it "Shifts both terms in an application" $
      termShift 2 (TmApp (TmAbs (TmApp (TmVar 0) (TmVar 1))) (TmVar 4)) `shouldBe`
      (TmApp (TmAbs (TmApp (TmVar 0) (TmVar 3))) (TmVar 6))
  describe "Lambda.Core.termSubst" $ do
    it "Substitutes a free variable for another one" $ property $
      \nnj ->
      \nns ->
        let j = getNonNegative nnj
	    s = TmVar (getNonNegative nns)
	in termSubst j s (TmVar j) == s
    it "Leaves bound variables alone when changing the same index for a free one" $ property $
      \nns ->
	let s = TmVar (getNonNegative nns)
        in termSubst 0 s (TmAbs (TmVar 0)) == (TmAbs (TmVar 0)) 

