module Semantics where

data Ty = TyArr Ty Ty
        | TyBool
        deriving (Eq, Show)

data Binding = NameBind
             | VarBind Ty
             deriving (Eq, Show)

type Context = [(String, Binding)]
