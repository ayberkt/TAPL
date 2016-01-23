{-# LANGUAGE UnicodeSyntax #-}

import Semantics

import Control.Monad.Loops (whileM_)
import System.IO (putStr, hFlush, getLine, stdout, hIsEOF, stdin)
import Parser (parseString)

main ∷ IO ()
main = do
  putStr "λ "
  hFlush stdout
  whileM_ (fmap not $ hIsEOF stdin) $ do
    hFlush stdout
    case parseString input of
      Left e  → putStrLn . show $ e
      Right t → putStrLn . show . eval $ t
    hFlush stdout
  putStrLn "Bye."
