{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO (putStr, hFlush, getLine, stdout)
import Parser (parseString)

main ∷ IO ()
main = do
  putStr "λ "
  hFlush stdout
  input ← getLine
  putStrLn $ show (parseString input)
  main
