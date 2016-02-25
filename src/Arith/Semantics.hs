{-# LANGUAGE UnicodeSyntax #-}

module Arith.Semantics (Term(..), isNumericVal, isVal, eval) where

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term

digitize ∷ Term → Int
digitize TmZero     = 0
digitize (TmSucc t) = 1 + digitize t

instance Show Term where
  show TmTrue         = "true"
  show TmFalse        = "false"
  show TmZero         = "0"
  show num@(TmSucc t) = show $ digitize num


isNumericVal ∷ Term → Term
isNumericVal TmZero     = TmTrue
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal _          = TmFalse

isVal ∷ Term → Term
isVal TmTrue  = TmTrue
isVal TmFalse = TmTrue
isVal t       = isNumericVal t

eval ∷ Term → Term
eval (TmIf TmTrue  t₂ t₃)   = t₂
eval (TmIf TmFalse t₂ t₃)   = t₃
eval (TmIf t₁      t₂ t₃)   = let t₁' = eval t₁
                               in TmIf t₁' t₂ t₃
eval (TmSucc t₁)            = let t₁' = eval t₁
                               in TmSucc t₁'
eval (TmPred TmZero)        = TmZero
eval (TmPred (TmSucc nv₁))  = nv₁
eval (TmPred t₁)            = let t₁' = eval t₁
                               in TmPred t₁'
eval (TmIsZero TmZero)      = TmTrue
eval (TmIsZero  _)          = TmFalse
eval t = t
