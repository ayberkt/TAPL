{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Control.Monad.Loops (whileM_)
import           System.IO           (getLine, hFlush, hIsEOF, putStr, stdin,
                                      stdout)
import           Typed.Parser        (parseExpr)
import           Typed.Semantics

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
