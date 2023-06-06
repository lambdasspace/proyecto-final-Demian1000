module PreParser
    ( PreRegex(..)
    , Terminal(..)
    , preParse
    ) where

import Token
import PreRegex
import Regex (Terminal(..))

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
        (preregex, [])    -> Just $ opUnario $ reversa preregex

opUnario :: PreRegex -> PreRegex
opUnario (SUBREGEX ls) =
    let
        recursive (SUBREGEX [z, OPERADOR INTERROGACION]) = SUBREGEX [recursive z, OPERADOR INTERROGACION]
        recursive (SUBREGEX [z, OPERADOR ESTRELLA]) = SUBREGEX [recursive z, OPERADOR ESTRELLA]
        recursive (SUBREGEX [z, OPERADOR MAS]) = SUBREGEX [recursive z, OPERADOR MAS]
        recursive (SUBREGEX [z, w]) = SUBREGEX [recursive z, recursive w]
        recursive x = opUnario x
    in
        case ls of
            [] -> SUBREGEX []
            [c] -> SUBREGEX [recursive c]
            (c:cs:css) -> case cs of
                (OPERADOR INTERROGACION) -> opUnario $ SUBREGEX $ SUBREGEX [recursive c, cs] :css
                (OPERADOR ESTRELLA) -> opUnario $ SUBREGEX $ SUBREGEX [recursive c, cs] :css
                (OPERADOR MAS) -> opUnario $ SUBREGEX $ SUBREGEX [recursive c, cs] :css
                _ -> bind (SUBREGEX [recursive c]) (opUnario $ SUBREGEX $ cs:css)
opUnario x = x


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
        INTERROGACION -> preParser (SUBREGEX $ OPERADOR INTERROGACION :xs) cs
        DISYUNCION    -> preParser (SUBREGEX $ OPERADOR DISYUNCION :xs) cs
        (SIMBOLO s)   -> preParser (SUBREGEX $ TERMINAL (Simbo s) :xs) cs
        ESTRELLA      -> preParser (SUBREGEX $ OPERADOR ESTRELLA :xs) cs
        PUNTO         -> preParser (SUBREGEX $ TERMINAL Punto :xs) cs
        EPSILON       -> preParser (SUBREGEX $ TERMINAL Epsil :xs) cs
        MAS           -> preParser (SUBREGEX $ OPERADOR MAS :xs) cs
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
                (corchetes, resto) -> preParser (SUBREGEX $ TERMINAL (Rango corchetes) :xs) resto
        RCOR -> err  -- Este caso nunca debería darse
preParser _ _ = err