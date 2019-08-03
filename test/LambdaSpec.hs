module LambdaSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Lambda.Core
import Lambda.Syntax

spec :: Spec
spec =  do
  describe "Lambda.Core.termShift" $ do
    it "is the identity when shifting by 0" $ termShift 0 (TmVar 0)  `shouldBe` (TmVar 0)

