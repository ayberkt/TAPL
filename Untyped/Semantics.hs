{-# LANGUAGE UnicodeSyntax #-}

data Term = TmVar Int
          | TmAbs Term
          | TmApp Term Term
