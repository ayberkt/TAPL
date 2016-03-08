{
module FullSimple.Tokens (tokenize, Token(..)) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-z]

tokens :-

  $white+                        ;
  "--".*                         ;
  lambda                         { \_ -> TokenLambda }
  if                             { \_ -> TokenIf     }
  then                           { \s -> TokenThen   }
  else                           { \s -> TokenElse   }
  .                              { \s -> TokenDot    }
  :                              { \s -> TokenColon  }
  true                           { \s -> TokenTrue   }
  false                          { \s -> TokenFalse  }
  unit                           { \s -> TokenUnit      }
  Unit                           { \s -> TokenUnitType  }
  Bool                           { \s -> TokenBoolType  }
  "->"                           { \s -> TokenArrowType  }
  \(                             { \s -> TokenLParen }
  \)                             { \s -> TokenRParen }
  $alpha+  { \s -> TokenSym s  }

{

data Token = TokenLambda
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenDot
           | TokenColon
           | TokenTrue
           | TokenFalse
           | TokenUnit
           | TokenUnitType
           | TokenBoolType
           | TokenArrowType
           | TokenLParen
           | TokenRParen
           | TokenSym String
           deriving (Eq, Show)

tokenize = alexScanTokens
}
