module Main (main) where

import Lexer
import PreParser
import Parser

main :: IO ()
main = do
  putStrLn "Enter a regular expression:"
  input <- getLine
  putStrLn "Input received. Tokenizing..."
  case lexer "abcdefghijklmnñopqrstuvwxyz" '-' input of
    Just tokens -> do
      case preParse  tokens of
        Just algo -> putStrLn $ ":" ++ show (parse algo)
        Nothing -> putStrLn "Error en el pre parseo."
    Nothing -> putStrLn "Invalid regular expression."