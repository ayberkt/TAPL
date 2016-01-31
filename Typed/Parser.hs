{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Semantics (Term(..))

import Text.ParserCombinators.Parsec (Parser(..)
                                     , ParseError
                                     , try
                                     , oneOf
                                     , char
                                     , digit
                                     , satisfy
                                     , many1
                                     , choice
                                     , chainl1)

import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L

lexer ∷ Tok.TokenParser ()
lexer = Tok.makeTokenParser
        $ L.emptyDef { T.reservedOpNames = ["lambda", ".", ":", "->"]
                     , T.reservedNames   = ["true", "false", "Bool"]
                     , T.opLetter        = oneOf ".:"}
variable ∷ Parser Term
variable = TmVar <$> identifier
