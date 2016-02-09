{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import           Semantics                     (NmTerm (..), Ty (..))

import           Text.ParserCombinators.Parsec (ParseError, Parser (..),
                                                alphaNum, chainl1, char, choice,
                                                digit, eof, letter, many1,
                                                oneOf, parse, satisfy, try)

import           Control.Applicative           ((<|>))
import qualified Text.Parsec.Expr              as E
import qualified Text.Parsec.Language          as L
import qualified Text.Parsec.Token             as T

------------
-- LEXING --
------------

lexer ∷ T.TokenParser ()
lexer = T.makeTokenParser
        $ L.emptyDef { T.identStart      = letter
                     , T.identLetter     = alphaNum
                     , T.reservedOpNames = ["lambda", ".", ":", "->"]
                     , T.reservedNames   = ["true", "false", "Bool"]
                     , T.opLetter        = oneOf ".:"
                     }

parens ∷ Parser a → Parser a
parens = T.parens lexer

natural ∷ Parser Integer
natural = T.natural lexer

reserved ∷ String → Parser ()
reserved = T.reserved lexer

reservedOp ∷ String → Parser ()
reservedOp = T.reservedOp lexer

identifier ∷ Parser String
identifier = T.identifier lexer

whiteSpace ∷ Parser ()
whiteSpace = T.whiteSpace lexer

-------------------------------------------------------------------------------
-------------------------------------- PARSING --------------------------------
-------------------------------------------------------------------------------
variable ∷ Parser NmTerm
variable = identifier >>= \x → return $ NmVar x

true ∷ Parser NmTerm
true = reserved "true" >> return NmTrue

false ∷ Parser NmTerm
false = reserved "false" >> return NmFalse

bool ∷ Parser NmTerm
bool = true <|> false

boolTy ∷ Parser Ty
boolTy = reserved "Bool" >> return TyBool

arrTy ∷ Parser Ty
arrTy = do
  τ1 ← anyType
  whiteSpace
  reservedOp "->"
  whiteSpace
  τ2 ← anyType
  return $ TyArr τ1 τ2

anyType ∷ Parser Ty
anyType =  boolTy
       <|> arrTy

abstraction ∷ Parser NmTerm
abstraction = do
  reservedOp "lambda"
  whiteSpace
  x ← identifier
  reservedOp ":"
  τ ← anyType
  reservedOp "."
  whiteSpace
  body ← expr
  return $ NmAbs x τ body

application ∷ Parser NmTerm
application = do
  f ← expr
  whiteSpace
  x ← expr
  return $ NmApp f x

expr ∷ Parser NmTerm
expr =  abstraction
    -- <|> application
    <|> variable
    <|> bool

parseExpr ∷ String → NmTerm
parseExpr t = case parse expr "" t of
                Left err  → error $ show err
                Right ast → ast
