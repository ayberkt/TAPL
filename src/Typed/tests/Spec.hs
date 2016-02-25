module Main where

import qualified Parser     as P
import           Semantics
import           Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "correctly parses" $
      P.parseExpr "lambda x:Bool. x"
      `shouldBe` NmAbs "x" TyBool (NmVar "x")
