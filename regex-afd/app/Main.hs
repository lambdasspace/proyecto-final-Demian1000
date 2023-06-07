module Main (main) where

import System.IO
import Lexer ( lexer )
import PreParser ( preParse )
import Parser ( parse )
import Afd ( shift, afd, markFirst )
import DrawAfd ( drawAfd, nodos )

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
                    write $ drawAfd (afd regex)
                    print algo
                    print (shift False (markFirst regex) 'a')
                    print (shift False (shift False (markFirst regex) 'a') 'a')
                    print (afd regex)
                    print $ nodos (afd regex)
                Nothing -> putStrLn "Error en el pre parseo."
        Nothing -> putStrLn "Invalid regular expression."

write str = do
    let file = "afd.svg"
    handle <- openFile file WriteMode
    hPutStrLn handle str
    hClose handle