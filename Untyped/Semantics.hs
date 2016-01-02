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
printtm ctx (TmAbs x t) = let (ctx', x') = pickfreshname ctx x
                              out = concat [ "(lambda "
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
printtm ctx (TmVar n) = if length ctx == n
                        then putStrLn (indexToName n)
                        else putStrLn "[bad index]"

pickfreshname ∷ Context → String → (Context, String )
pickfreshname = undefined

indexToName ∷ Int → String
indexToName = undefined

ctxlength ∷ Context → Int
ctxlength = length
