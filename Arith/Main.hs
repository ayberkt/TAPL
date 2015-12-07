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
  putStrLn $ show (eval $ parseString input)
  main
