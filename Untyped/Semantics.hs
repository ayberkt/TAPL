{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}

module Semantics where

data Binding = NameBind deriving (Eq, Ord, Show)

type Context = [(String, Binding)]

data Term = TmVar Int         -- The representation of a variable is
                              -- just a number.
          | TmAbs String Term -- The representation of an abstraction
                              -- carries just a subterm for the
                              -- abstraction's body.
          | TmApp Term Term   -- An application carries the two subterms
                              -- being applied.
          deriving (Eq, Ord, Show)

printtm ∷ Context → Term → IO ()
printtm ctx (TmVar n) = if length ctx == n
                        then putStrLn (indexToName n)
                        else putStrLn "[bad index]"
printtm ctx (TmAbs x t) = let (ctx', x') = pickFreshName ctx x
                              out        = concat [ "(lambda "
                                                  , show x
                                                  , ". "
                                                  , show ctx'
                                                  , show t
                                                  , show ")"]
                        in putStrLn out
printtm ctx (TmApp t₁ t₂) = let out = concat [ "("
                                             , show ctx
                                             , show t₁
                                             , show ctx
                                             , show t₁
                                             , ")" ]
                            in putStrLn out

pickFreshName ∷ Context → String → (Context, String )
pickFreshName ctx x = if isNameBound ctx x
                      then pickFreshName ctx (x ++ "'")
                      else (((x, NameBind) : ctx), x)

isNameBound ∷ Context → String → Bool
isNameBound []          x = False
isNameBound ((y, _):ys) x = if y == x
                            then True
                            else isNameBound ys x

-- TODO: Implement
indexToName ∷ Int → String
indexToName = undefined

-- `ctxlength` is merely an alas for `length` that we create
-- for the sake of consistency with TAPL.
ctxlength ∷ Context → Int
ctxlength = length
