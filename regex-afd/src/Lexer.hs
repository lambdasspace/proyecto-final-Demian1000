{-# LANGUAGE InstanceSigs #-}
module Lexer
  ( Token(..)
  , lexer
  ) where

data Token
    = SIMBOLO Char
    | ESTRELLA
    | DISYUNCION
    | MAS
    | INTERROGACION
    | NEGACION
    | PUNTO
    | LPAR
    | RPAR
    | LCOR
    | RCOR
    | EPSILON
    deriving (Eq)

instance Show Token where
    show :: Token -> String
    show (SIMBOLO c) = [c]
    show ESTRELLA = "*"
    show DISYUNCION = "|"
    show MAS = "+"
    show INTERROGACION = "?"
    show NEGACION = "~"
    show PUNTO = "."
    show LPAR = "("
    show RPAR = ")"
    show LCOR = "["
    show RCOR = "]"
    show EPSILON = "ε"

-- Ignora los espacios en blanco
-- Convierte en tokens toda la cadena de entrada
-- Si hay un símbolo no permitido manda error.
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

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\t', '\n', '\r', '\v']