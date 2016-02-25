{-# LANGUAGE UnicodeSyntax #-}

module Typed.Semantics where

import           Control.Arrow ((***))

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

addBinding ∷ Context → String → Binding → Context
addBinding ctx x bind = (x, bind) : ctx

getTypeFromContext ∷ Context → Int → Either String Ty
getTypeFromContext ctx i
  = case getBinding ctx i of
      VarBind tyT → Right tyT
      _           → Left msg
  where msg = "Wrong kind of binding for variable"

getBinding ∷ Context → Int → Binding
getBinding ctx i = snd $ ctx !! i

-- Unfortunately, GHC doesn't accept Γ as a valid
-- variable name so we use γ instead :(
typeOf ∷ Context → Term → Either String Ty
typeOf γ (TmVar i _) = getTypeFromContext γ i
typeOf γ (TmAbs x τ1 t)
  = let γ' = addBinding γ x (VarBind τ1)
        τ' = typeOf γ' t
    in case τ' of
         Right τ2 → Right $ TyArr τ1 τ2
         -- TODO: Fix error message.
         _        → Left "Something went wrong."
typeOf γ (TmApp t1 t2)
  = let (τ1', τ2') = (typeOf γ) *** (typeOf γ) $ (t1, t2)
    in case (τ1', τ2') of
         (Right τ1, Right τ2)
           → case τ1 of
           (TyArr _ β2)
             → if τ2 == β2
               then Right β2
               else Left "Parameter type mismatch"
           _ → Left "Parameter type mismatch."
         _ → error "Case not defined."
typeOf _ TmTrue = Right TyBool
typeOf _ TmFalse = Right TyBool
typeOf γ (TmIf t1 t2 t3)
  | typeOf γ t1 == Right TyBool
      = let τ2' = typeOf γ t2
        in case τ2' of
        Right τ2 → if τ2' == (typeOf γ t3)
                   then Right τ2
                   else Left armsDifferentMsg
        err → err
  | otherwise = Left guardNotABoolMsg
  where armsDifferentMsg = "Arms of conditional have different types."
        guardNotABoolMsg = "Guard of conditional not a boolean."
