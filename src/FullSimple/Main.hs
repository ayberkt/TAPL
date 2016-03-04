{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Control.Monad.Loops  (whileM_)
import           FullSimple.Parser    (parseExpr)
import           FullSimple.Semantics (Binding (NameBind), Context, Term (..),
                                       Ty (..), eval, removeNames, typeOf)
import           System.IO            (hFlush, hIsEOF, stdin, stdout)


main ∷ IO ()
main = do
  putStr "λ "
  hFlush stdout
  whileM_ (fmap not $ hIsEOF stdin) $ do
    hFlush stdout
    input ← getLine
    let expr = removeNames [] (parseExpr input)
    case typeOf [] expr of
      Right τ → let expr' = eval [] expr
                    exprWType = (exprToString [] $ expr')
                                ++ " : " ++ typeToString τ
                in putStrLn exprWType
      Left  err  → putStrLn err
    putStr "λ "
    hFlush stdout
  putStrLn "Bye."


parens ∷ String → String
parens s = "(" ++ s ++ ")"

typeToString ∷ Ty → String
typeToString (TyArr τ1 τ2)
  = typeToString τ1 ++ " → " ++ typeToString τ2
typeToString TyBool = "Bool"
typeToString TyUnit = "Unit"
typeToString TyBase = "Base"

isNameBound ∷ Context → String → Bool
isNameBound [] _ = False
isNameBound ((y,_):bs) x
  | y == x = True
  | otherwise = isNameBound bs x

pickFreshName ∷ Context → String → (Context, String)
pickFreshName ctx x
  | isNameBound ctx x = pickFreshName ctx (x ++ "'")
  | otherwise = let ctx' = ((x, NameBind) : ctx)
                in (ctx', x)

exprToString ∷ Context → Term → String
exprToString ctx (TmVar x)
  = case ctx !! x of (x', _) → x'
exprToString ctx (TmAbs x τ t)
  = let (ctx', x') = pickFreshName ctx x
    in parens $ "λ" ++ x' ++ " : " ++ typeToString τ ++ ". " ++ exprToString ctx' t
exprToString ctx (TmApp t1 t2)
  = exprToString ctx t1 ++ " " ++ exprToString ctx t2
exprToString _ TmTrue
  = "true"
exprToString _ TmFalse
  = "false"
exprToString _ TmUnit = "unit"
exprToString ctx (TmIf t1 t2 t3)
  = let t1str = exprToString ctx t1
        t2str = exprToString ctx t2
        t3str = exprToString ctx t3
    in "if " ++ t1str ++
       " then " ++ t2str ++
       " else " ++ t3str
