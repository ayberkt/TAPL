{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Monad.Loops (whileM_)
import System.IO (putStr, hFlush, getLine, stdout, hIsEOF, stdin)
import Parser (parseString)
import Semantics (eval)

main ∷ IO ()
main = do
  putStr "λ "
  hFlush stdout
  whileM_ (fmap not $ hIsEOF stdin) $ do
    hFlush stdout
    input ← getLine
    case parseString input of
        Left  e → putStrLn . show $ e
        Right t → putStrLn . show . eval $ t
    putStr "λ "
    hFlush stdout
  putStrLn "Bye."
