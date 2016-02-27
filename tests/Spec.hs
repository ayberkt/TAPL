module Main where

import           Test.Hspec
import qualified Typed.Parser    as P
import           Typed.Semantics (Binding(NameBind), NmTerm (..), Term (..),
                                  Ty (..), removeNames)


main :: IO ()
main = hspec $ do
  describe "Typed.Parser can parse variables:" $ do
    let input1 = "x"
    it ("correctly parses " ++ input1) $
      P.parseExpr input1 `shouldBe` NmVar "x"
    let input2 = "ayberk"
    it ("correctly parses " ++ input2) $
      P.parseExpr input2 `shouldBe` NmVar "ayberk"
  describe "Typed.Parser can parse abstractions:" $ do
    let input1 = "lambda x:Bool. x"
    it ("correctly parses " ++ input1) $
      P.parseExpr input1 `shouldBe` NmAbs "x" TyBool (NmVar "x")
    let input2 = "lambda at:Bool. at"
    it ("correctly parses " ++ input2) $
      P.parseExpr input2 `shouldBe` NmAbs "at" TyBool (NmVar "at")
    let input4 = "lambda x:Bool -> Bool. x"
    it ("correctly parses " ++ input4) $
      P.parseExpr input4 `shouldBe` NmAbs "x" (TyArr TyBool TyBool) (NmVar "x")
    let input5 = "lambda x:Bool -> Bool -> Bool. x"
    it ("correctly parses " ++ input5) $
      P.parseExpr input5
      `shouldBe`
      NmAbs "x" (TyArr (TyArr TyBool TyBool) TyBool) (NmVar "x")
    let input6 = "lambda x : Bool. x"
    it ("correctly parses " ++ input6) $
      P.parseExpr input6 `shouldBe` NmAbs "x" TyBool (NmVar "x")
    let input7 = "lambda x : Bool . x"
    it ("correctly parses " ++ input7) $
      P.parseExpr input7 `shouldBe` NmAbs "x" TyBool (NmVar "x")
  describe "Typed.Parser can parse applications:" $ do
    let input4 = "(lambda x:Bool. x) (lambda x:Bool. x)"
    it ("correctly parses " ++ input4) $
      P.parseExpr input4
      `shouldBe` let f = NmAbs "x" TyBool (NmVar "x") in NmApp f f
    let input5 = "f g x"
    it ("correctly parses " ++ input5) $
      P.parseExpr input5
      `shouldBe` NmApp (NmApp (NmVar "f") (NmVar "g")) (NmVar "x")
    let input6 = "(lambda x:Bool. x) (lambda x:Bool. x) (lambda x:Bool. x)"
    it ("correctly parses " ++ input6) $
      P.parseExpr input6
      `shouldBe` let f = NmAbs "x" TyBool (NmVar "x") in NmApp (NmApp f f) f
    let input7 = "(lambda x:Bool. x) x"
    it ("correctly parses " ++ input7) $
      P.parseExpr input7
      `shouldBe` (NmApp (NmAbs "x" TyBool (NmVar "x"))) (NmVar "x")
    let input8 = "f g h x"
    it ("correctly parses " ++ input8) $
      P.parseExpr input8
      `shouldBe` NmApp (NmApp
                        (NmApp (NmVar "f") (NmVar "g"))
                        (NmVar "h"))
                       (NmVar "x")
  describe "Typed.Semantics --- removeNames:" $ do
    let input1 = NmAbs "x" TyBool (NmVar "x")
    it ("can handle " ++ show input1) $ do
      removeNames [] input1 `shouldBe` TmAbs "x" TyBool (TmVar 0)
    let input2 = NmAbs "y" TyBool
                 $ NmAbs "x" TyBool (NmApp (NmVar "x") (NmVar "y"))
    it ("can handle " ++ show input2) $ do
      removeNames [] input2
      `shouldBe` TmAbs "y" TyBool (TmAbs "x" TyBool (TmApp (TmVar 0) (TmVar 1)))
    let input3 = NmAbs "y" TyBool
                 $ NmAbs "x" TyBool (NmApp (NmVar "y") (NmVar "x"))
    it ("can handle " ++ show input3) $ do
      removeNames [] input3
      `shouldBe` TmAbs "y" TyBool (TmAbs "x" TyBool (TmApp (TmVar 1) (TmVar 0)))
    let input4 = NmAbs "x" TyBool (NmAbs "x" TyBool (NmVar "x"))
    it ("can handle " ++ show input3) $ do
      removeNames [] input4
      `shouldBe` TmAbs "x" TyBool (TmAbs "x" TyBool (TmVar 0))
    let input5 = NmApp (NmVar "x") (NmApp (NmVar "y") (NmVar "z"))
        ctx5 = zip ["b", "a", "z", "y", "x"] (repeat NameBind)
    it ("can handle " ++ show input5) $ do
      removeNames ctx5 input5
      `shouldBe`
      TmApp (TmVar 4) (TmApp (TmVar 3) (TmVar 2))
    let input6 = NmAbs "w" TyBool (NmApp (NmVar "y") (NmVar "w"))
        ctx6 = zip ["b", "a", "z", "y", "x"] (repeat NameBind)
    it ("can handle " ++ show input6) $ do
      removeNames ctx6 input6
      `shouldBe`
      TmAbs "w" TyBool (TmApp (TmVar 4) (TmVar 0))
    let input7 = NmAbs "w" TyBool (NmAbs "a" TyBool (NmVar "x"))
        ctx7 = zip ["b", "a", "z", "y", "x"] (repeat NameBind)
    it ("can handle " ++ show input7) $ do
      removeNames ctx7 input7
      `shouldBe`
      TmAbs "w" TyBool (TmAbs "a" TyBool (TmVar 6))
    let input8 = NmAbs "s" TyBool
                  (NmAbs "z" TyBool
                   (NmApp (NmVar "s")
                    (NmApp (NmVar "s") (NmVar "z"))))
    it ("can handle " ++ show input7) $ do
      removeNames [] input8
      `shouldBe`
      TmAbs "s" TyBool
        (TmAbs "z" TyBool
         (TmApp (TmVar 1)
          (TmApp (TmVar 1) (TmVar 0))))

