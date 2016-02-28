{-# LANGUAGE UnicodeSyntax #-}

module Typed.Semantics where

import           Data.List       (elemIndex)


data Ty = TyArr Ty Ty
        | TyBool
        deriving (Eq, Show)

data Term = TmVar Int
          | TmAbs String Ty Term
          | TmApp Term Term
          | TmTrue
          | TmFalse
          | TmIf Term Term Term
          deriving (Eq, Show)

data NmTerm = NmVar String
            | NmAbs String Ty NmTerm
            | NmApp NmTerm NmTerm
            | NmTrue
            | NmFalse
            | NmIf NmTerm NmTerm NmTerm
            deriving (Eq, Show)

data Binding = NameBind
             | VarBind Ty
             deriving (Eq, Show)

type Context = [(String, Binding)]

removeNames :: Context → NmTerm → Term
removeNames ctx (NmVar x)
  = case elemIndex (x, NameBind) ctx of
      Just n -> TmVar n
      Nothing -> error ("Variable " ++ x ++ " is not bound.")
removeNames ctx (NmAbs x t e)
  = let ctx' = (x, NameBind) : ctx
    in TmAbs x t $ removeNames ctx' e
removeNames ctx (NmApp e1 e2)
  = TmApp (removeNames ctx e1) (removeNames ctx e2)
removeNames ctx (NmIf e1 e2 e3)
  = TmIf (removeNames ctx e1) (removeNames ctx e2) (removeNames ctx e3)
removeNames _ NmTrue = TmTrue
removeNames _ NmFalse = TmFalse

addBinding ∷ Context → String → Binding → Context
addBinding ctx x bind = (x, bind) : ctx

getTypeFromContext ∷ Context → Int → Either String Ty
getTypeFromContext ctx i
  = case getBinding ctx i of
      VarBind tyT →  Right tyT
      _           → Left "Wrong kind of binding for variable."

getBinding ∷ Context → Int → Binding
getBinding ctx i = case (ctx !! i) of (_, b) -> b

typeOf ∷ Context → Term → Either String Ty

typeOf ctx (TmVar i) = getTypeFromContext ctx i

typeOf ctx (TmAbs x τ1 t)
  = let ctx' = addBinding ctx x (VarBind τ1)
    in case typeOf ctx' t of
         Right τ2 → Right $ TyArr τ1 τ2
         err → err

typeOf ctx (TmApp t1 t2)
  = let Right τ1 = typeOf ctx t1
        Right τ2 = typeOf ctx t2
    in case τ1 of
         TyArr τ1_1 τ1_2
           → if τ2 == τ1_1
             then Right τ1_2
             else Left "Parameter type mismatch."
         _ → Left "Arrow type expected."

typeOf ctx (TmIf t1 t2 t3)
  | typeOf ctx t1 == Right TyBool
      = let τ2 = typeOf ctx t2
        in if τ2 == typeOf ctx t3
           then τ2
           else Left "Arms of conditional have different types."
  | otherwise = Left "Guard of conditional is not a boolean."

typeOf _ TmTrue = Right TyBool
typeOf _ TmFalse = Right TyBool
