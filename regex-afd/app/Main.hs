module Main (main) where

import Lexer
import PreParser

main :: IO ()
main = do
  putStrLn "Enter a regular expression:"
  input <- getLine
  putStrLn "Input received. Tokenizing..."
  case lexer "abcdefghijklmnñopqrst" '-' input of
    Just tokens -> do
      case preParse  tokens of
        Just algo -> putStrLn $ "pre parseo lol:" ++ show algo
        Nothing -> putStrLn "Error en el pre parseo."
    Nothing -> putStrLn "Invalid regular expression."