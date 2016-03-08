{
module FullSimple.Parse (parseExpr) where
import FullSimple.Tokens (Token(..), tokenize)
import FullSimple.Semantics (NmTerm(..), Ty(..))
}

%name expr
%tokentype { Token      }
%error     { parseError }

%token
    if       { TokenIf        }
    then     { TokenThen      }
    else     { TokenElse      }
    VAR      { TokenSym $$    }
    lambda   { TokenLambda    }
    true     { TokenTrue      }
    false    { TokenFalse     }
    unit     { TokenUnit      }
    Bool     { TokenBoolType  }
    Unit     { TokenUnitType  }
    '->'     { TokenArrowType }
    '.'      { TokenDot       }
    ':'      { TokenColon     }
    '('      { TokenLParen    }
    ')'      { TokenRParen    }

%left  '->'
%%

Expr : if Expr then Expr else Expr        { NmIf  $2 $4 $6 }
     | lambda VAR ':' Type '.' Expr       { NmAbs $2 $4 $6 }
     | Atom                               { $1             }

Term : Term Term                          { NmApp $1 $2    }
     | Atom                               { $1             }

Atom   : '(' Expr ')'                     { $2             }
       | true                             { NmTrue         }
       | false                            { NmFalse        }
       | unit                             { NmUnit         }
       | VAR                              { NmVar $1       }

Type : Type '->' Type  { TyArr $1 $3 }
     | AtomicType      { $1          }

AtomicType : Bool { TyBool }
           | Unit { TyUnit }

{
parseError :: [Token] -> a
parseError _ = error "Parse error."

parseExpr :: String -> NmTerm
parseExpr = expr . tokenize
}