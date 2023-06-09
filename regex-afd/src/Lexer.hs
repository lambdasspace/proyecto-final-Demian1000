{- | Módulo para hacer el análisis léxico de la entrada.

Esta es la primera fase para procesar la cadena de entrada,
se verifica que todos los caracteres de entrada sean válidos
y se genera una lista con los Tokens de entrada.
-}
module Lexer
  ( lexer
  ) where

import Token

{- | Revisa que todos los caracteres de entrada sean válidos
y genera una lista de tokens si no hay errores.

Ignora los espacios en blanco.

Si hay un símbolo no permitido regresa Nothing.

El primer parámetro es el alfabeto permitido.
El segundo parámetro es el caracter correspondiente a epsilon.
El tercer parámetro es la cadena de entrada.
-}
lexer :: [Char] -> Char -> String -> Maybe [Token]
lexer _ _ [] = Just []
lexer alfabeto epsilon (c:cs)
    | c == epsilon      = (EPSILON :)       <$> lexer alfabeto epsilon cs
    | c `elem` alfabeto = (SIMBOLO c :)     <$> lexer alfabeto epsilon cs
    | c == '*'          = (ESTRELLA :)      <$> lexer alfabeto epsilon cs
    | c == '|'          = (DISYUNCION :)    <$> lexer alfabeto epsilon cs
    | c == '+'          = (MAS :)           <$> lexer alfabeto epsilon cs
    | c == '?'          = (INTERROGACION :) <$> lexer alfabeto epsilon cs
    | c == '~'          = (NEGACION :)      <$> lexer alfabeto epsilon cs
    | c == '.'          = (PUNTO :)         <$> lexer alfabeto epsilon cs
    | c == '('          = (LPAR :)          <$> lexer alfabeto epsilon cs
    | c == ')'          = (RPAR :)          <$> lexer alfabeto epsilon cs
    | c == '['          = (LCOR :)          <$> lexer alfabeto epsilon cs
    | c == ']'          = (RCOR :)          <$> lexer alfabeto epsilon cs
    | isWhitespace c    = lexer alfabeto epsilon cs
    | otherwise         = Nothing

-- | Detecta si un caracter corresponde a un espacio en blanco.
isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\t', '\n', '\r', '\v']