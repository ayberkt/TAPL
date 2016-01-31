{-# LANGUAGE UnicodeSyntax #-}

module Semantics where

import Control.Arrow ((***))

type TypeMismatch = Nothing

data Ty = TyArr Ty Ty
        | TyBool
        deriving (Eq, Show)


data Term = TmVar Int Int
          | TmAbs String Ty Term
          | TmApp Term Term
          | TmTrue
          | TmFalse
          | TmIf Term Term Term
          deriving (Eq, Show)

data Binding = NameBind
             | VarBind Ty
             deriving (Eq, Show)

type Context = [(String, Binding)]

addBinding ∷ Context → String → Binding → Context
addBinding ctx x bind = (x, bind) : ctx

getTypeFromContext ∷ Context → Int → Maybe Ty
getTypeFromContext ctx i = case getBinding ctx i of
                             VarBind tyT → Just tyT
                             _           → TypeMismatch

getBinding ∷ Context → Int → Binding
getBinding ctx i = snd $ ctx !! i

-- Unfortunately, GHC doesn't accept Γ as a valid
-- variable name so we use γ instead :(
typeOf ∷ Context → Term → Either String Ty
typeOf γ (TmVar i _) = getTypeFromContext γ i
typeOf γ (TmAbs x τ₁ t) = let γ' = addBinding γ x (VarBind τ₁)
                              τ' = typeOf γ' t
                         in case τ' of Just τ₂ → Right $ TyArr τ₁ τ₂
typeOf γ (TmApp t₁ t₂) = let ττ@(τ₁', τ₂') = (typeOf γ) *** (typeOf γ) $ (t₁, t₂)
                         in case ττ of
                              (Just τ₁, Just τ₂)
                                → case τ₁ of
                                    (TyArr β₁ β₂)
                                      → if τ₂ == β₂
                                        then Just β₂
                                        else Right "Parameter type mismatch"
                              _ → Left "Parameter type mismatch."
typeOf _ TmTrue = Just TyBool
typeOf _ TmFalse = Just TyBool
typeOf γ (TmIf t₁ t₂ t₃)
  | typeOf γ t₁ == TyBool = let τ₂ = typeOf γ t₂
                            in if τ₂ == typeOf γ t₃
                               then Right τ₂
                               else Left armsDifferentMsg
  | otherwise = Left guardNotABoolMsg
  where armsDifferentMsg = "Arms of conditional have different types."
        guardNotABoolMsg = "Guard of conditional not a boolean."
