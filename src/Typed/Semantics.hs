{-# LANGUAGE UnicodeSyntax #-}

module Typed.Semantics where

import           Data.List (elemIndex)


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

termShift ∷ Int → Int → Term → Term
termShift d c (TmVar k)
  = if k < c then TmVar k else TmVar (k + d)
termShift d c (TmAbs _ _ t)
  = termShift d (succ c) t
termShift d c (TmApp t1 t2)
  = TmApp (termShift d c t1) (termShift d c t2)
termShift _ _  TmTrue = TmTrue
termShift _ _ TmFalse = TmFalse
termShift d c (TmIf t1 t2 t3)
  = TmIf (termShift d c t1) (termShift d c t2) (termShift d c t3)


termSubst ∷ Int → Term → Term → Term
termSubst j s (TmVar k)
  = if k == j then s else (TmVar k)
termSubst j s (TmAbs x τ t)
  = TmAbs x τ $ termSubst (succ j) (termShift 1 0 s) t
termSubst j s (TmApp t1 t2)
  = TmApp (termSubst j s t1) (termSubst j s t2)
termSubst _ _  TmTrue
  = TmTrue
termSubst _ _ TmFalse
  = TmFalse
termSubst j s (TmIf t1 t2 t3)
  = TmIf (termSubst j s t1) (termSubst j s t2) (termSubst j s t3)


isVal ∷ Term → Bool
isVal TmTrue = True
isVal TmFalse = True
isVal (TmAbs _ _ _) = True
isVal (TmVar _) = True
isVal _ = False

termSubstTop ∷ Term → Term → Term
termSubstTop s t = termShift (-1) 0 (termSubst 0 (termShift 1 0 s) t)

eval ∷ Context → Term → Term
eval ctx (TmApp t1 t2)
  = case t1 of
      (TmAbs _ _ t1_2)
        → if isVal t2
          then termSubstTop t2 t1_2
          else if isVal t1
               then let t2' = eval ctx t2 in (TmApp t1 t2')
               else let t1' = eval ctx t1 in (TmApp t1' t2)
      _ → if isVal t1
          then let t2' = eval ctx t2 in (TmApp t1 t2')
          else let t1' = eval ctx t1 in (TmApp t1' t2)
eval _ (TmIf TmTrue t2  _) = t2
eval _ (TmIf TmFalse _ t3) = t3
eval ctx (TmIf t1 t2 t3)
  = let t1' = eval ctx t1
    in TmIf t1' t2 t3
eval _ t = t
