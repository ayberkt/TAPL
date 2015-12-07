{-# LANGUAGE UnicodeSyntax #-}

module Parser where

-- import qualified Semantics as S
import Prelude hiding (succ, pred)
import Semantics(Term(..))
import Text.ParserCombinators.Parsec ((<|>), string, char, Parser(..))

term ∷ Parser Term
term =  zero
    <|> succ
    <|> pred
    <|> iszero

value ∷ Parser Term
value = true <|> false <|> numericVal

numericVal ∷ Parser Term
numericVal = zero <|> pred <|> succ

true ∷ Parser Term
true = do
  string "true"
  return TmTrue

false ∷ Parser Term
false = do
  string "false"
  return TmFalse

conditional ∷ Parser Term
conditional = do
  string "if"
  t₁ ← term
  string "then"
  t₂ ← term
  string "else"
  t₃ ← term
  return $ TmIf t₁ t₂ t₃

zero ∷ Parser Term
zero = do
  char '0'
  return TmZero

succ ∷ Parser Term
succ = do
  string "succ"
  t ← term
  return $ TmSucc t

pred ∷ Parser Term
pred = do
  string "pred"
  t ← term
  return $ TmPred t

iszero ∷ Parser Term
iszero = do
  string "iszero"
  t ← term
  return $ TmIsZero t
