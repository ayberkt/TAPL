{-# LANGUAGE UnicodeSyntax #-}

module Arithmetic where

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term

isNumericVal ∷ Term → Term
isNumericVal TmZero     = TmTrue
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal _          = TmFalse

isVal ∷ Term → Term
isVal TmTrue  = TmTrue
isVal TmFalse = TmTrue
isVal t       = isNumericVal t

eval₁ (TmIf TmTrue  t₂ t₃)   = t₂
eval₁ (TmIf TmFalse t₂ t₃)   = t₃
eval₁ (TmIf t₁      t₂ t₃)   = let t₁' = eval₁ t₁
                               in TmIf t₁' t₂ t₃
eval₁ (TmSucc t₁)            = let t₁' = eval₁ t₁
                               in TmSucc t₁'
eval₁ (TmPred TmZero)        = TmZero
eval₁ (TmPred (TmSucc nv₁))  = nv₁
eval₁ (TmPred t₁)            = let t₁' = eval₁ t₁
                               in TmPred t₁'
eval₁ (TmIsZero TmZero)      = TmTrue
eval₁ (TmIsZero  _)          = TmFalse
eval₁ (TmIsZero t₁)          = let t₁' = eval₁ t₁
                               in TmIsZero t₁'

eval t = let t' = eval₁ t
         in eval t'
