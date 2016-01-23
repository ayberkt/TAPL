{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}

module Semantics where

data Binding = NameBind deriving (Eq, Ord, Show)

type Context = [(String, Binding)]

data Term = TmVar Int          -- The representation of a variable is
                               -- just a number.
          | TmAbs String Term  -- The representation of an abstraction
                               -- carries just a subterm for the
                               -- abstraction's body.
          | TmApp Term Term    -- An application carries the two subterms
                               -- being applied.
          deriving (Eq, Ord, Show)

termShift ∷ Int → Term → Term
termShift d t =
  let walk c term
        = case term of
            TmVar x → if x >= c
                      then TmVar $ x + d
                      else TmVar x
            TmAbs x t₁ → TmAbs x $ walk (succ c) t₁
            TmApp t₁ t₂ → TmApp (walk c t₁) (walk c t₂)
  in walk 0 t

termSubst ∷ Int → Term → Term → Term
termSubst j s t =
  let walk c term
        = case term of
            TmVar x → if x == j + c
                      then termShift c s
                      else TmVar x
            TmAbs x t₁ → TmAbs x (walk (c + 1) t₁)
            TmApp t₁ t₂ → TmApp (walk c t₁) (walk c t₂)
  in walk 0 t

termSubstTop ∷ Term → Term → Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal ∷ Context → Term → Bool
isVal ctx (TmAbs _ _) = True
isVal ctx _           = False

eval₁ ∷ Context → Term → Term
eval₁ ctx (TmApp t₁ t₂)
  | isVal ctx t₂ = let (TmAbs _ t₁₂) = t₁
                   in termSubstTop t₂ t₁₂
  | isVal ctx t₁ = let t₂' = eval₁ ctx t₂
                   in TmApp t₁ t₂'
  | otherwise = let t₁' = eval₁ ctx t₁
                in TmApp t₁' t₂
eval₁ _ t = t

eval ∷ Context → Term → Term
eval ctx t = let t' = eval₁ ctx t
             in eval ctx t'
