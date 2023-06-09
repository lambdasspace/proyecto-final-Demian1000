{- | Módulo para hacer pre procesamientos en la lista de tokens
antes de generar su expresión regular.

Las transformaciones que hace este módulo facilita la construcción
del Regex, y garantiza que los operadores estén bien aplicados,
y que los paréntesis o corchetes estén balanceados.
-}
module PreParser
    ( PreRegex(..)
    , Terminal(..)
    , preParse
    ) where

import Token
import PreRegex
import Regex (Terminal(..))

-- | Aplica las funciones para obtener PreRegex a partir de una lista de Tokens.
preParse :: [Token] -> Maybe PreRegex
preParse tokens =
    case preParser (SUBREGEX []) tokens of
        (SUBREGEX [], []) -> Nothing
        (_, _:_)          -> Nothing
        (preregex, [])    -> Just $ opUnario $ reversa preregex

{- | Recorre la estructura PreRegex, buscando operadores unarios,
mete los operadores unarios junto con la expresión a la que se le aplica
en sub-expresiones SUBREGEX; de esta nos aseguramos que los operadores
se estén aplicando en su alcance correcto.
-}
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

-- | Invierte el orden de una PreRegex.
reversa :: PreRegex -> PreRegex
reversa (SUBREGEX l) = SUBREGEX $ map reversa (reverse l)
reversa x = x

-- | Constante para representar los errores en la función.
err :: (PreRegex, [a])
err = (SUBREGEX [], [])

{- | Trnasforma una lista de Tokens en una PreRegex, mientras revisa
que la lista de tokens sea válida.
Revisa que los operadores están bien aplicados, y crea subexpresiones para
encapsular las expresiones dentro de los paréntesis, (para que se respete la
precedencia posteriormente).
También se asegura que los paréntesis y los corchetes estén balanceados,
y que dentro de los corchetes solo haya caracteres.

Devuelve el PreRegex al revés.

Si devuelve (SUBREGEX [], []) significa que la entrada era inválida.

El primer parámetro es el acumulador de la PreRegex.
El segundo parámetro es la lista de tokens que se está leyendo.
-}
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