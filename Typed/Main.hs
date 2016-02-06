{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Monad.Loops (whileM_)
import System.IO (putStr, hFlush, getLine, stdout, hIsEOF, stdin)
import Semantics
import Parser (parseExpr)

main ∷ IO ()
main = do
  putStr "λ "
  hFlush stdout
  whileM_ (fmap not $ hIsEOF stdin) $ do
    hFlush stdout
    input ← getLine
    let result     = parseExpr input
    putStrLn . show $ result
    putStr "λ "
    hFlush stdout
  putStrLn "Bye."
