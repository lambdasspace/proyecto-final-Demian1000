module Main (main) where

import System.IO
import Lexer ( lexer )
import PreParser ( preParse )
import Parser ( parse )
import Afd ( afd )
import DrawAfd ( drawAfd )

{- | Ejecuta el programa, pide una cadena de entrada,
y devuelve un mensaje si ocurre un error.

Si no ocurre ningún error, guarda el AFD en afd.svg .
-}
main :: IO ()
main = do
    putStrLn "Ingresa una expresión regular ʕ•́ᴥ•̀ʔっ:"
    input <- getLine
    putStrLn "Entrada recibida. Procesando..."
    case lexer "abcdefghijklmnñopqrstuvwxyz" '-' input of -- el alfabeto y epsilon
        Just tokens -> do
            case preParse  tokens of
                Just algo -> do
                    let regex = parse algo
                    write $ drawAfd (afd regex)
                    putStrLn "Listo ! ᕙ(`▿´)ᕗ"
                Nothing -> putStrLn "Expresión inválida (>‿◠)✌"
        Nothing -> putStrLn "Expresión inválida (>‿◠)✌"

-- | Escribe una cadena en el archivo afd.svg
write :: String -> IO()
write str = do
    let file = "afd.svg"
    handle <- openFile file WriteMode
    hPutStrLn handle str
    hClose handle