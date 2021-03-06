module Main where

import           Test.Hspec
import qualified Typed.Parser    as P
import qualified FullSimple.Parse as FP
import FullSimple.Semantics
-- import           Typed.Semantics (Binding(..), NmTerm (..), Term (..),
                                  -- Ty (..), removeNames, typeOf, eval)


main :: IO ()
main = do
  testFullSimple
  -- hspec $ d-- o
    -- describe "Typed.Parser can parse variables:" $ do
    --   let input1 = "x"
    --   it ("correctly parses " ++ input1) $
    --     P.parseExpr input1 `shouldBe` NmVar "x"
    --   let input2 = "ayberk"
    --   it ("correctly parses " ++ input2) $
    --     P.parseExpr input2 `shouldBe` NmVar "ayberk"

    -- describe "Typed.Parser can parse abstractions:" $ do
    --   let input1 = "lambda x:Bool. x"
    --   it ("correctly parses " ++ input1) $
    --     P.parseExpr input1 `shouldBe` NmAbs "x" TyBool (NmVar "x")
    --   let input2 = "lambda at:Bool. at"
    --   it ("correctly parses " ++ input2) $
    --     P.parseExpr input2 `shouldBe` NmAbs "at" TyBool (NmVar "at")
    --   let input4 = "lambda x:Bool -> Bool. x"
    --   it ("correctly parses " ++ input4) $
    --     P.parseExpr input4 `shouldBe` NmAbs "x" (TyArr TyBool TyBool) (NmVar "x")
    --   let input5 = "lambda x:Bool -> Bool -> Bool. x"
    --   it ("correctly parses " ++ input5) $
    --     P.parseExpr input5
    --     `shouldBe`
    --     NmAbs "x" (TyArr (TyArr TyBool TyBool) TyBool) (NmVar "x")
    --   let input6 = "lambda x : Bool. x"
    --   it ("correctly parses " ++ input6) $
    --     P.parseExpr input6 `shouldBe` NmAbs "x" TyBool (NmVar "x")
    --   let input7 = "lambda x : Bool . x"
    --   it ("correctly parses " ++ input7) $
    --     P.parseExpr input7 `shouldBe` NmAbs "x" TyBool (NmVar "x")

    -- describe "Typed.Parser can parse applications:" $ do
    --   let input4 = "(lambda x:Bool. x) (lambda x:Bool. x)"
    --   it ("correctly parses " ++ input4) $
    --     P.parseExpr input4
    --     `shouldBe` let f = NmAbs "x" TyBool (NmVar "x") in NmApp f f
    --   let input5 = "f g x"
    --   it ("correctly parses " ++ input5) $
    --     P.parseExpr input5
    --     `shouldBe` NmApp (NmApp (NmVar "f") (NmVar "g")) (NmVar "x")
    --   let input6 = "(lambda x:Bool. x) (lambda x:Bool. x) (lambda x:Bool. x)"
    --   it ("correctly parses " ++ input6) $
    --     P.parseExpr input6
    --     `shouldBe` let f = NmAbs "x" TyBool (NmVar "x") in NmApp (NmApp f f) f
    --   let input7 = "(lambda x:Bool. x) x"
    --   it ("correctly parses " ++ input7) $
    --     P.parseExpr input7
    --     `shouldBe` (NmApp (NmAbs "x" TyBool (NmVar "x"))) (NmVar "x")
    --   let input8 = "f g h x"
    --   it ("correctly parses " ++ input8) $
    --     P.parseExpr input8
    --     `shouldBe` NmApp (NmApp
    --                       (NmApp (NmVar "f") (NmVar "g"))
    --                       (NmVar "h"))
    --                     (NmVar "x")

    -- describe "Typed.Semantics --- removeNames:" $ do
    --   let input1 = NmAbs "x" TyBool (NmVar "x")
    --   it ("can handle " ++ show input1) $ do
    --     removeNames [] input1 `shouldBe` TmAbs "x" TyBool (TmVar 0)
    --   let input2 = NmAbs "y" TyBool
    --               $ NmAbs "x" TyBool (NmApp (NmVar "x") (NmVar "y"))
    --   it ("can handle " ++ show input2) $ do
    --     removeNames [] input2
    --     `shouldBe` TmAbs "y" TyBool (TmAbs "x" TyBool (TmApp (TmVar 0) (TmVar 1)))
    --   let input3 = NmAbs "y" TyBool
    --               $ NmAbs "x" TyBool (NmApp (NmVar "y") (NmVar "x"))
    --   it ("can handle " ++ show input3) $ do
    --     removeNames [] input3
    --     `shouldBe` TmAbs "y" TyBool (TmAbs "x" TyBool (TmApp (TmVar 1) (TmVar 0)))
    --   let input4 = NmAbs "x" TyBool (NmAbs "x" TyBool (NmVar "x"))
    --   it ("can handle " ++ show input3) $ do
    --     removeNames [] input4
    --     `shouldBe` TmAbs "x" TyBool (TmAbs "x" TyBool (TmVar 0))
    --   let input5 = NmApp (NmVar "x") (NmApp (NmVar "y") (NmVar "z"))
    --       ctx5 = zip ["b", "a", "z", "y", "x"] (repeat NameBind)
    --   it ("can handle " ++ show input5) $ do
    --     removeNames ctx5 input5
    --     `shouldBe`
    --     TmApp (TmVar 4) (TmApp (TmVar 3) (TmVar 2))
    --   let input6 = NmAbs "w" TyBool (NmApp (NmVar "y") (NmVar "w"))
    --       ctx6 = zip ["b", "a", "z", "y", "x"] (repeat NameBind)
    --   it ("can handle " ++ show input6) $ do
    --     removeNames ctx6 input6
    --     `shouldBe`
    --     TmAbs "w" TyBool (TmApp (TmVar 4) (TmVar 0))
    --   let input7 = NmAbs "w" TyBool (NmAbs "a" TyBool (NmVar "x"))
    --       ctx7 = zip ["b", "a", "z", "y", "x"] (repeat NameBind)
    --   it ("can handle " ++ show input7) $ do
    --     removeNames ctx7 input7
    --     `shouldBe`
    --     TmAbs "w" TyBool (TmAbs "a" TyBool (TmVar 6))
    --   let input8 = NmAbs "s" TyBool
    --                 (NmAbs "z" TyBool
    --                 (NmApp (NmVar "s")
    --                   (NmApp (NmVar "s") (NmVar "z"))))
    --   it ("can handle " ++ show input7) $ do
    --     removeNames [] input8
    --     `shouldBe`
    --     TmAbs "s" TyBool
    --       (TmAbs "z" TyBool
    --       (TmApp (TmVar 1)
    --         (TmApp (TmVar 1) (TmVar 0))))
    -- describe "Type checker" $ do
    --   let expr1 = TmAbs "x" TyBool (TmVar 0)
    --   it ("accepts " ++ show expr1) $ do
    --     typeOf [] expr1 `shouldBe` (Right $ TyArr TyBool TyBool)
    --   let expr2 = TmApp expr1 (TmVar 1)
    --       ctx2  = [("x", NameBind), ("x", VarBind TyBool)]
    --   it ("accepts " ++ show expr2) $ do
    --     typeOf ctx2 expr2 `shouldBe` Right TyBool
    --   let expr3 = TmAbs "y" TyBool (TmAbs "x" TyBool (TmApp expr1 (TmVar 0)))
    --   it ("accepts " ++ show expr3) $ do
    --     typeOf [] expr3 `shouldBe` (Right $ TyArr TyBool (TyArr TyBool TyBool))
    --   let expr4 = TmIf (TmVar 0) expr1 expr1
    --   it ("rejects " ++ show expr4) $ do
    --     typeOf [("x", VarBind (TyArr TyBool TyBool))] expr4
    --     `shouldBe` (Left "Guard of conditional is not a boolean.")
    --   let expr5 = TmAbs "x" TyBool (TmIf (TmVar 0) expr1 (TmVar 1))
    --   it ("rejects " ++ show expr5) $ do
    --     typeOf [("k", VarBind TyBool)] expr5
    --     `shouldBe` (Left $ "Arms of conditional have different types.")
    -- describe "Evaluator" $ do
    --   let expr1 = TmVar 0
    --   it ("correctly evaluates " ++ show expr1) $ do
    --     eval [("z", VarBind TyBool)] expr1
    --     `shouldBe` (TmVar 0)
    --   let expr2 = (TmApp (TmAbs "x" TyBool (TmVar 0))) (TmVar 0)
    --   it ("correctly evaluates " ++ show expr1) $ do
    --     eval [("z", VarBind TyBool)] expr2
    --     `shouldBe` (TmVar 0)
    --   let expr3 = let f = (TmAbs "x" (TyArr TyBool TyBool) (TmVar 0))
    --                   x = (TmAbs "foo" TyBool (TmVar 0))
    --               in TmApp f x
    --   it ("correctly evaluates " ++ show expr3) $ do
    --     eval [] expr3
    --     `shouldBe` (TmAbs "foo" TyBool (TmVar 0))
    --   let expr4 = TmAbs "foo" TyBool (TmVar 0)

    --   it ("correctly evaluates " ++ show expr4) $ do
    --     eval [] expr4
    --     `shouldBe` (TmAbs "foo" TyBool (TmVar 0))

testFullSimple :: IO ()
testFullSimple = hspec $ do
  describe "FullSimple.Parser can parse variables:" $ do
    let input1 = "x"
    it ("correctly parses " ++ input1) $
      FP.parseExpr input1 `shouldBe` NmVar "x"
    let input2 = "ayberk"
    it ("correctly parses " ++ input2) $
      FP.parseExpr input2 `shouldBe` NmVar "ayberk"

  describe "FullSimple.Parser can parse abstractions:" $ do
    let input1 = "lambda x:Bool. x"
    it ("correctly parses " ++ input1) $
      FP.parseExpr input1 `shouldBe` NmAbs "x" TyBool (NmVar "x")
    let input2 = "lambda at:Bool. at"
    it ("correctly parses " ++ input2) $
      FP.parseExpr input2 `shouldBe` NmAbs "at" TyBool (NmVar "at")
    let input4 = "lambda x:Bool -> Bool. x"
    it ("correctly parses " ++ input4) $
      FP.parseExpr input4 `shouldBe` NmAbs "x" (TyArr TyBool TyBool) (NmVar "x")
    let input5 = "lambda x:Bool -> Bool -> Bool. x"
    it ("correctly parses " ++ input5) $
      FP.parseExpr input5
      `shouldBe`
      NmAbs "x" (TyArr (TyArr TyBool TyBool) TyBool) (NmVar "x")
    let input6 = "lambda x : Bool. x"
    it ("correctly parses " ++ input6) $
      FP.parseExpr input6 `shouldBe` NmAbs "x" TyBool (NmVar "x")
    let input7 = "lambda x : Bool . x"
    it ("correctly parses " ++ input7) $
      FP.parseExpr input7 `shouldBe` NmAbs "x" TyBool (NmVar "x")

  describe "Typed.Parser can parse applications:" $ do
    let input4 = "(lambda x:Bool. x) (lambda x:Bool. x)"
    it ("correctly parses " ++ input4) $
      FP.parseExpr input4
      `shouldBe` let f = NmAbs "x" TyBool (NmVar "x") in NmApp f f
    let input5 = "f g x"
    it ("correctly parses " ++ input5) $
      FP.parseExpr input5
      `shouldBe` NmApp (NmApp (NmVar "f") (NmVar "g")) (NmVar "x")
    let input6 = "(lambda x:Bool. x) (lambda x:Bool. x) (lambda x:Bool. x)"
    it ("correctly parses " ++ input6) $
      FP.parseExpr input6
      `shouldBe` let f = NmAbs "x" TyBool (NmVar "x") in NmApp (NmApp f f) f
    let input7 = "(lambda x:Bool. x) x"
    it ("correctly parses " ++ input7) $
      FP.parseExpr input7
      `shouldBe` (NmApp (NmAbs "x" TyBool (NmVar "x"))) (NmVar "x")
    let input8 = "f g h x"
    it ("correctly parses " ++ input8) $
      FP.parseExpr input8
      `shouldBe` NmApp (NmApp
                        (NmApp (NmVar "f") (NmVar "g"))
                        (NmVar "h"))
                       (NmVar "x")
    let input9 = "(lambda x : Bool. lambda y : Bool. x) true false"
    it ("correctly parses " ++ input9) $
      FP.parseExpr input9
      `shouldBe` let g = (NmAbs "y" TyBool (NmVar "x"))
                     f = (NmAbs "x" TyBool g)
                 in NmApp (NmApp f NmTrue) NmFalse
