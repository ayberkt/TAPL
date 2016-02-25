module Main where

import qualified Typed.Parser     as P
import           Typed.Semantics
import           Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Typed.Parser" $ do
    let input1 = "lambda x:Bool. x"
    it ("correctly parses " ++ input1) $
      P.parseExpr input1 `shouldBe` NmAbs "x" TyBool (NmVar "x")
    let input2 = "lambda at:Bool. at"
    it ("correctly parses " ++ input2) $
      P.parseExpr input2 `shouldBe` NmAbs "at" TyBool (NmVar "at")
    let input3 = "x"
    it ("correctly parses " ++ input3) $
      P.parseExpr input3 `shouldBe` NmVar "x"
