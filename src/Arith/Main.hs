{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Arith.Parser        (parseString)
import           Arith.Semantics     (eval)
import           Control.Monad.Loops (whileM_)
import           System.IO           (getLine, hFlush, hIsEOF, putStr, stdin,
                                      stdout)

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
