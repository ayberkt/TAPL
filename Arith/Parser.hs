{-# LANGUAGE UnicodeSyntax #-}

module Parser where

-- import qualified Semantics as S
import           Prelude                             hiding (pred, succ)
import           Semantics                           (Term (..))
import           Text.ParserCombinators.Parsec       (Parser (..), char, parse,
                                                      string, skipMany, (<|>), space)

term ∷ Parser Term
term = do
    skipMany space
    t ← zero <|> value <|> conditional <|> iszero
    skipMany space
    return t

value ∷ Parser Term
value = true <|> false <|> numericValue

numericValue ∷ Parser Term
numericValue = zero <|> pred <|> succ

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
  skipMany $ char ' '
  string "succ"
  t ← term
  return $ TmSucc t

pred ∷ Parser Term
pred = do
  skipMany $ char ' '
  string "pred"
  t ← term
  return $ TmPred t

iszero ∷ Parser Term
iszero = do
  string "iszero"
  t ← term
  return $ TmIsZero t

parseString :: [Char] -> Term
parseString str = case parse term "" str of
                    Left  e → error $ show e
                    Right r → r
