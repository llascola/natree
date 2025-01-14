module Main where

import qualified Parser (parse)

main :: IO ()
main = do
  -- lee una linea de texto y la parsea
  -- en un AST de SProp
  s <- getLine
  print $ Parser.parse s
  putStrLn "Hello, Haskell!"
