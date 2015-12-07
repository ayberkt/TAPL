{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO (putStr, hFlush, getLine, stdout)
import Parser (parseString)
import Semantics (eval)

main ∷ IO ()
main = do
  putStr "λ "
  hFlush stdout
  input ← getLine
  case parseString input of
    Left  e → putStrLn . show $ e
    Right t → putStrLn . show . eval $ t
  main
