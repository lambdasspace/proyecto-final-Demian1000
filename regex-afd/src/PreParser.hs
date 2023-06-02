{-# LANGUAGE InstanceSigs #-}
module PreParser
    ( PreRegex
    , preParse
    ) where

import Lexer (Token(..))

-- Tipo recursivo para poder tener listas de Tokens dentro de listas de tokens, etc.
data PreRegex
    = OPERADOR Token
    | SUBREGEX [PreRegex]
    | TERMINAL Terminal

-- tipo de dato para encapsular todo slos tipos de terminales
data Terminal
    = Rango String
    | Negac Char
    | Simbo Char
    | Punto
    | Epsil

-- forma d ever para debuggeo
instance Show PreRegex where
    show :: PreRegex -> String
    show (OPERADOR o) = show o
    show (SUBREGEX ls) = show ls
    show (TERMINAL t) = show t

-- forma de ver para debuggeo
instance Show Terminal where
    show :: Terminal -> String
    show (Rango r) = "[" ++ r ++ "]"
    show (Negac n) = "~" ++ [n]
    show (Simbo s) = [s]
    show Punto = "."
    show Epsil = "ε"

--parentesis balanceados
--corchetes bien ponidos
-- operadores bien aplicados
-- distintos niveles de expresones parentisadas
-- terminales encapsulados

-- Con esto construir el ASA esta ez pez
--reversa de la lista

preParse :: [Token] -> Maybe PreRegex
preParse tokens =
    case preParser (SUBREGEX []) tokens of
        (SUBREGEX [], []) -> Nothing
        (_, _:_)          -> Nothing
        (preregex, [])    -> Just $ reversa preregex

reversa :: PreRegex -> PreRegex
reversa (SUBREGEX l) = SUBREGEX $ map reversa (reverse l)
reversa x = x

-- Definimos una constante para los errores
err :: (PreRegex, [a])
err = (SUBREGEX [], [])

-- Devuelve la lista al revés
-- Si devuelve err la entrada era inválida
preParser :: PreRegex -> [Token] -> (PreRegex, [Token])
preParser (SUBREGEX []) [] = err  -- La expresión regular vacia es un error
preParser subregex [] = (subregex, []) -- Caso base, si terminamos de leer todos los tokens
preParser (SUBREGEX []) (ESTRELLA:_) = err -- Los operadores siempre deben estar aplicados
preParser (SUBREGEX []) (DISYUNCION:_) = err -- Los operadores siempre deben estar aplicados
preParser (SUBREGEX []) (MAS:_) = err -- Los operadores siempre deben estar aplicados
preParser (SUBREGEX []) (INTERROGACION:_) = err -- Los operadores siempre deben estar aplicados
preParser (SUBREGEX xs) (c:cs) = 
    case c of
        (SIMBOLO s)   -> preParser (SUBREGEX $ TERMINAL (Simbo s) :xs) cs
        ESTRELLA      -> preParser (SUBREGEX $ OPERADOR ESTRELLA :xs) cs
        DISYUNCION    -> preParser (SUBREGEX $ OPERADOR DISYUNCION :xs) cs
        MAS           -> preParser (SUBREGEX $ OPERADOR MAS :xs) cs
        INTERROGACION -> preParser (SUBREGEX $ OPERADOR INTERROGACION :xs) cs
        PUNTO         -> preParser (SUBREGEX $ TERMINAL Punto :xs) cs
        EPSILON       -> preParser (SUBREGEX $ TERMINAL Epsil :xs) cs
        NEGACION      -> case cs of
            (SIMBOLO s : css) -> preParser (SUBREGEX $ TERMINAL (Negac s):xs) css
            _                 -> err

        LPAR ->
            let parentesisAux = preParser (SUBREGEX []) cs
            in case parentesisAux of
                (SUBREGEX [], [])        -> err
                (parentesis, RPAR:resto) -> preParser (SUBREGEX $ parentesis:xs) resto -- Revisamos que la expresion cerró sus paréntesis
                _                        -> err
        RPAR -> (SUBREGEX xs, c:cs) -- No sacamos el paréntesis, para poder detectar errores.

        LCOR -> 
            let consumeCad zs ((SIMBOLO s):ws) = consumeCad (zs ++ [s]) ws
                consumeCad zs (RCOR:ws) = (zs, ws)
                consumeCad _ _ = ([], [])
                corchetesAux = consumeCad [] cs
            in case corchetesAux of
                ([], [])           -> err
                (corchetes, resto) -> preParser (TERMINAL (Rango corchetes)) resto
        RCOR -> err  -- Este caso nunca debería darse

preParser _ _                  = err