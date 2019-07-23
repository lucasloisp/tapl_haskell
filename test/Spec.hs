import Test.Hspec
import Control.Exception (evaluate)
import Core
import Syntax

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

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


