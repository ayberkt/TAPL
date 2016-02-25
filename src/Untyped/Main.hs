{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Untyped.Semantics

import           Control.Monad.Loops (whileM_)
import           System.IO           (getLine, hFlush, hIsEOF, putStr, stdin,
                                      stdout)
import           Untyped.Parser      (parseString)

main ∷ IO ()
main = do
  putStr "λ "
  hFlush stdout
  whileM_ (fmap not $ hIsEOF stdin) $ do
    hFlush stdout
    input ← getLine
    case parseString input of
      Left  e  → putStrLn . show $ e
      Right t → putStrLn . show $ t
    hFlush stdout
  putStrLn "Bye."
