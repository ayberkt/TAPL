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
  "."                             { \_ -> TokenDot         }
  ","                             { \_ -> TokenComma       }
  ":"                             { \_ -> TokenColon       }
  true                            { \_ -> TokenTrue        }
  false                           { \_ -> TokenFalse       }
  unit                            { \_ -> TokenUnit        }
  Unit                            { \_ -> TokenUnitType    }
  Bool                            { \_ -> TokenBoolType    }
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
           | TokenDot
           | TokenComma
           | TokenColon
           | TokenTrue
           | TokenFalse
           | TokenUnit
           | TokenUnitType
           | TokenBoolType
           | TokenArrowType
           | TokenProductType
           | TokenLParen
           | TokenRParen
           | TokenSym String
           deriving (Eq, Show)

tokenize = alexScanTokens
}
