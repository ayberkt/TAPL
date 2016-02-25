{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Control.Monad.Loops (whileM_)
import           System.IO           (hFlush, hIsEOF, stdin, stdout)
import           Typed.Parser        (parseExpr)

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
