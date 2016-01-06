{-# LANGUAGE UnicodeSyntax #-}

import Semantics

import Control.Monad.Loops (whileM_)
import System.IO (putStr, hFlush, getLine, stdout, hIsEOF, stdin)

main ∷ IO ()
main = do
  putStr "λ "
  hFlush stdout
  whileM_ (fmap not $ hIsEOF stdin) $ do
    hFlush stdout
    input ← getLine
    putStrLn $ "You typed " ++ input
    putStr "λ "
    hFlush stdout
  putStrLn "Bye."
