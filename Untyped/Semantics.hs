{-# LANGUAGE UnicodeSyntax #-}

module Semantics where

type Binding = NameBind

type Context = [(String, Binding)]

data Term = TmVar Int  -- The representation of a variable is just a number.
          | TmAbs Term -- The representation of an abstraction carries just a
                       -- just a subterm for the abstraction's body.
          | TmApp Term Term -- An application carries the two subterms
                            -- being applied.

printtm ∷ Term → IO ()
printtm ctx (TmAbs t) = let (ctx', x') = pickfreshname ctx x
                        in putStrLn "(lambda " ++ (show x)
                                               ++ ". "
                                               ++ (show ctx')
                                               ++ (show t)
                                               ++ (show ")")
printtm ctx (TmApp t₁ t₂) = putStrLn "(" ++ (show ctx)
                                        ++ (show t₁)
                                        ++ " "
                                        ++ (show ctx)
                                        ++ (show t₁)
                                        ++ ")"
printtm ctx (TmVar n) = if ctxlength ctx == n
                        then putStrLn (index2name n)
                        else putStrLn "[bad index]"

pickfreshname = undefined

index2name = undefined

ctxlength ctx = undefined
