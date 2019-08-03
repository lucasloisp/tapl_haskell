module LambdaSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Arith.Core
import Arith.Syntax

spec :: Spec
spec =  do
  describe "Lib.eval1" $ do
    it "has zero as normal"  $ eval1 (TmZero ()) `shouldBe` Nothing

