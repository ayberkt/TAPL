{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import           Semantics                     (NmTerm (..))
import           Text.ParserCombinators.Parsec (Parser(..)
                                               , ParseError
                                               , oneOf
                                               , char
                                               , parse
                                               , string
                                               , skipMany
                                               , (<|>)
                                               , space)

term ∷ Parser NmTerm
term =  abstraction
    <|> application

parseVariable ∷ Parser String
parseVariable = do
  skipMany space
  x ← oneOf ['a'..'z']
  return $ pure  x

variable ∷ Parser NmTerm
variable = do
  x ← parseVariable
  return $ NmVar x

application ∷ Parser NmTerm
application = do
  t₁ ← term
  skipMany space
  t₂ ← term
  return $ NmApp t₁ t₂

abstraction ∷ Parser NmTerm
abstraction = do
  string "lambda"
  skipMany space
  x ← parseVariable
  string "."
  skipMany space
  t ← term
  return $ NmAbs x t

parseString ∷ [Char] → Either ParseError NmTerm
parseString input = parse term "" input
