module Main where

import System.Environment (getArgs)
import MP

main :: IO ()
main = do
  args <- getArgs
  case args of
    [template, source, output] -> do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    _ -> putStrLn "Usage: cabal run macroprocessor <template> <info> <output>"
