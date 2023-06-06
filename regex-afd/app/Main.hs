module Main (main) where

import Lexer ( lexer )
import PreParser ( preParse )
import Parser ( parse )
import Afd ( shift )

main :: IO ()
main = do
  putStrLn "Enter a regular expression:"
  input <- getLine
  putStrLn "Input received. Tokenizing..."
  case lexer "abcdefghijklmnñopqrstuvwxyz" '-' input of
    Just tokens -> do
      case preParse  tokens of
        Just algo -> do
          let regex = parse algo
          print algo
          print (shift True regex 'g')
        Nothing -> putStrLn "Error en el pre parseo."
    Nothing -> putStrLn "Invalid regular expression."