module ArithSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Arith.Core
import Arith.Syntax

spec :: Spec
spec =  do
  describe "Lib.eval1" $ do
    it "has zero as normal"  $ eval1 (TmZero ()) `shouldBe` Nothing
    it "has true as normal"  $ eval1 (TmTrue ()) `shouldBe` Nothing
    it "has false as normal" $ eval1 (TmFalse ()) `shouldBe` Nothing
    it "evaluates an if-true" $
      eval1 (TmIf () (TmTrue ()) (TmZero ()) (TmSucc () (TmZero ())))
        `shouldBe` Just (TmZero ())
    it "evaluates an if-false" $
      eval1 (TmIf () (TmFalse ()) (TmZero ()) (TmSucc () (TmZero ())))
        `shouldBe` Just (TmSucc () (TmZero ()))
    it "evaluates an if's clause" $
      eval1 (TmIf () (TmIf () (TmTrue ()) (TmZero ()) (TmSucc () (TmZero ()))) (TmZero ()) (TmSucc () (TmZero ())))
        `shouldBe` Just (TmIf () (TmZero ()) (TmZero ()) (TmSucc () (TmZero ())))
    it "evaluates the predecessor of zero" $
      eval1 (TmPred () (TmZero ())) `shouldBe` Just (TmZero ())
    it "evaluates the predecessor of one" $
      eval1 (TmPred () (TmSucc () (TmZero ()))) `shouldBe` Just (TmZero ())

  describe "Lib.eval" $ do
    it "evaluates the value zero" $ eval (TmZero ()) `shouldBe` (TmZero ())
    it "evaluates the value false" $ eval (TmFalse ()) `shouldBe` (TmFalse ())
    it "evaluates the value true" $ eval (TmTrue ()) `shouldBe` (TmTrue ())
    it "evaluates an if-true" $
      eval (TmIf () (TmTrue ()) (TmZero ()) (TmSucc () (TmZero ())))
        `shouldBe` (TmZero ())
    it "evaluates an if-false" $
      eval (TmIf () (TmFalse ()) (TmZero ()) (TmSucc () (TmZero ())))
        `shouldBe` (TmSucc () (TmZero ()))
    it "evaluates an if statement" $
      eval (TmIf () (TmIf () (TmTrue ()) (TmFalse ()) (TmTrue ())) (TmZero ()) (TmSucc () (TmZero ())))
        `shouldBe` (TmSucc () (TmZero ()))
    it "evaluates the predecessor of zero" $
      eval (TmPred () (TmZero ())) `shouldBe` (TmZero ())
    it "evaluates the predecessor of one" $
      eval (TmPred () (TmSucc () (TmZero ()))) `shouldBe` (TmZero ())
    it "evaluates a complex if clause" $
      eval (TmIf () (TmIsZero () (TmPred () (TmSucc () (TmZero ()))))
        (TmSucc () (TmPred () (TmSucc () (TmZero ())))) (TmZero ()))
        `shouldBe` TmSucc () (TmZero ())

  describe "Lib.evalBigStep" $ do
    it "evaluates the value zero" $ evalBig (TmZero ()) `shouldBe` (TmZero ())
    it "evaluates the value false" $ evalBig (TmFalse ()) `shouldBe` (TmFalse ())
    it "evaluates the value true" $ evalBig (TmTrue ()) `shouldBe` (TmTrue ())
    it "evaluates an if-true" $
      evalBig (TmIf () (TmTrue ()) (TmZero ()) (TmSucc () (TmZero ())))
        `shouldBe` (TmZero ())
    it "evaluates an if-false" $
      evalBig (TmIf () (TmFalse ()) (TmZero ()) (TmSucc () (TmZero ())))
        `shouldBe` (TmSucc () (TmZero ()))
    it "evaluates an if statement" $
      evalBig (TmIf () (TmIf () (TmTrue ()) (TmFalse ()) (TmTrue ())) (TmZero ()) (TmSucc () (TmZero ())))
        `shouldBe` (TmSucc () (TmZero ()))
    it "evaluates the predecessor of zero" $
      evalBig (TmPred () (TmZero ())) `shouldBe` (TmZero ())
    it "evaluates the predecessor of one" $
      evalBig (TmPred () (TmSucc () (TmZero ()))) `shouldBe` (TmZero ())
    it "evaluates a complex if clause" $
      evalBig (TmIf () (TmIsZero () (TmPred () (TmSucc () (TmZero ()))))
        (TmSucc () (TmPred () (TmSucc () (TmZero ())))) (TmZero ()))
        `shouldBe` TmSucc () (TmZero ())


