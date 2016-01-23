{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import           Semantics                     (Term (..))
import           Text.ParserCombinators.Parsec (Parser(..)
                                               , ParseError
                                               , char
                                               , parse
                                               , string
                                               , skipMany
                                               , (<|>)
                                               , space)

term ∷ Parser Term
term =  variable
    <|> abstraction
    <|> application

variable ∷ Parser Term
variable = string "x" <|> string "y" <|> string "z"

application ∷ Parser Term
application = do
  t₁ ← term
  string " "
  t₂ ← term
  return $ TmApp t₁ t₂

abstraction ∷ Parser Term
abstraction = do
  string "lambda"
  x ← variable
  string "."
  t ← term
  return $ TmAbs x t

parseString ∷ [Char] → Either ParseError Term
parseString input = parse term "" input
