{
module FullSimple.Tokens (tokenize, Token(..)) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-z]
$eol   = [\n]

tokens :-

  $eol                             ;
  $white+                         ;
  "--".*                          ;
  lambda                          { \_ -> TokenLambda      }
  if                              { \_ -> TokenIf          }
  then                            { \_ -> TokenThen        }
  else                            { \_ -> TokenElse        }
  as                              { \_ -> TokenAs          }
  let                             { \_ -> TokenLet         }
  in                              { \_ -> TokenIn          }
  "="                             { \_ -> TokenEq          }
  "."                             { \_ -> TokenDot         }
  ","                             { \_ -> TokenComma       }
  ":"                             { \_ -> TokenColon       }
  ";"                             { \_ -> TokenSequence    }
  true                            { \_ -> TokenTrue        }
  false                           { \_ -> TokenFalse       }
  unit                            { \_ -> TokenUnit        }
  Unit                            { \_ -> TokenUnitType    }
  Bool                            { \_ -> TokenBoolType    }
  Base                            { \_ -> TokenBaseType    }
  "->"                            { \_ -> TokenArrowType   }
  "*"                             { \_ -> TokenProductType }
  \(                              { \_ -> TokenLParen      }
  \)                              { \_ -> TokenRParen      }
  $alpha  [$alpha $digit  \_ \']* { \s -> TokenSym s       }

{

data Token = TokenLambda
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenAs
           | TokenLet
           | TokenIn
           | TokenEq
           | TokenDot
           | TokenComma
           | TokenColon
           | TokenSequence
           | TokenTrue
           | TokenFalse
           | TokenUnit
           | TokenUnitType
           | TokenBoolType
           | TokenBaseType
           | TokenArrowType
           | TokenProductType
           | TokenLParen
           | TokenRParen
           | TokenSym String
           deriving (Eq, Show)

tokenize = alexScanTokens
}
