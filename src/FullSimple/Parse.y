{
module FullSimple.Parse (parseExpr) where
import FullSimple.Tokens (Token(..), tokenize)
import FullSimple.Semantics (NmTerm(..), Ty(..))
}

%name expr
%tokentype { Token      }
%error     { parseError }

%token
    if       { TokenIf          }
    then     { TokenThen        }
    else     { TokenElse        }
    VAR      { TokenSym $$      }
    lambda   { TokenLambda      }
    true     { TokenTrue        }
    false    { TokenFalse       }
    as       { TokenAs          }
    unit     { TokenUnit        }
    Bool     { TokenBoolType    }
    Unit     { TokenUnitType    }
    Base     { TokenBaseType    }
    '->'     { TokenArrowType   }
    '*'      { TokenProductType }
    '.'      { TokenDot         }
    ','      { TokenComma       }
    ';'      { TokenSequence    }
    ':'      { TokenColon       }
    '('      { TokenLParen      }
    ')'      { TokenRParen      }

%left  '->'
%%

Expr : if Expr then Expr else Expr        { NmIf  $2 $4 $6  }
     | lambda VAR ':' Type '.' Expr       { NmAbs $2 $4 $6  }
     | Term                               { $1              }

Term : Term Atom                          { NmApp $1 $2     }
     | Term as Type                       { NmAscribe $1 $3 }
     | Sequence                           { $1              }

Sequence : Sequence ';' Atom              { NmSeq $1 $3     }
         | Pair                           { $1              }

Pair : '(' Expr ',' Expr ')'              { NmPair $2 $4    }
     | Atom                               { $1              }

Atom   : '(' Expr ')'                     { $2              }
       | VAR                              { NmVar  $1       }
       | true                             { NmTrue          }
       | false                            { NmFalse         }
       | unit                             { NmUnit          }

Type : Type '->' Type  { TyArr  $1 $3 }
     | Type_           { $1           }

Type_ : Type_ '*' Type_ { TyProd $1 $3 }
      | AtomicType      { $1           }

AtomicType : Bool { TyBool }
           | Unit { TyUnit }
           | Base { TyBase }

{
parseError :: [Token] -> a
parseError _ = error "Parse error."

parseExpr :: String -> NmTerm
parseExpr = expr . tokenize
}
