{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Semantics ( NmTerm(..)
                 , Ty(..))

import Text.ParserCombinators.Parsec (Parser(..)
                                     , ParseError
                                     , try
                                     , oneOf
                                     , char
                                     , digit
                                     , satisfy
                                     , many1
                                     , choice
                                     , chainl1
                                     , eof)

import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Expr as E
import Control.Applicative ((<|>))

------------
-- LEXING --
------------

lexer ∷ T.TokenParser ()
lexer = T.makeTokenParser
        $ L.emptyDef { T.reservedOpNames = ["lambda", ".", ":", "->"]
                     , T.reservedNames   = ["true", "false", "Bool"]
                     , T.opLetter        = oneOf ".:"}

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

allOf ∷ Parser a → Parser a
allOf p = do
  T.whiteSpace lexer
  r ← p
  eof
  return r

-------------
-- PARNG --
-------------

variable ∷ Parser NmTerm
variable = NmVar <$> T.identifier

anyType ∷ Parser Ty
anyType = let atomicType = parens anyType <|> boolType
              table = [[E.Infix (TyArr <$ reservedOp "->") E.AssocRight]]
              boolType = TyBool <$ reserved "Bool"
          in E.buildExpressionParser table atomicType

bool ∷ Parser NmTerm
bool = let tru = NmTrue <$ reserved "true"
           fls = NmFalse <$ reserved "false"
       in tru <|> fls

atomicExpr ∷ Parser NmTerm
atomicExpr = parens expr
          <|> variable
          <|> lambda
          <|> bool

expr ∷ Parser NmTerm
expr = foldl1 NmApp <$> many1 atomicExpr

lambda ∷ Parser NmTerm
lambda = NmAbs <$> (reservedOp "lambda" *> identifier)
               <*> (reservedOp ":" *> anyType)
               <*> (reservedOp "." *> expr)
