{-# LANGUAGE UnicodeSyntax #-}

module FullSimple.Parser where

import           Control.Applicative           ((<|>))
import           FullSimple.Semantics          (NmTerm (..), Ty (..))
import qualified Text.Parsec.Language          as L
import qualified Text.Parsec.Token             as T
import           Text.ParserCombinators.Parsec (Parser, alphaNum, chainl1,
                                                letter, oneOf, parse)

------------
-- LEXING --
------------

lexer ∷ T.TokenParser ()
lexer = T.makeTokenParser
        $ L.emptyDef { T.identStart      = letter
                     , T.identLetter     = alphaNum
                     , T.reservedOpNames = ["lambda", ".", ":", "->"]
                     , T.reservedNames   = ["true", "false", "unit", "Bool", "Unit"]
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

unit ∷ Parser NmTerm
unit = reserved "unit" >> return NmUnit

bool ∷ Parser NmTerm
bool = true <|> false

boolTy ∷ Parser Ty
boolTy = reserved "Bool" >> return TyBool

unitTy ∷ Parser Ty
unitTy = reserved "Unit" >> return TyUnit

arrTy ∷ Parser Ty
arrTy = let arrTy' = do { reservedOp "->"; return TyArr }
        in boolTy `chainl1` arrTy'

anyType ∷ Parser Ty
anyType = arrTy <|> boolTy <|> unitTy

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
application = let f = do { whiteSpace; return NmApp}
              in nonApp `chainl1` f

nonApp ∷ Parser NmTerm
nonApp = parens expr
      <|> abstraction
      <|> variable
      <|> true
      <|> false
      <|> unit

expr ∷ Parser NmTerm
expr = application <|> nonApp

parseExpr ∷ String → NmTerm
parseExpr t = case parse expr "" t of
                Left err  → error $ show err
                Right ast → ast
