{- | Tipo de dato para encapsular los caracteres válidos
dentro de una expresión regular, representan todos los
operadores y los símbolos terminales.

Sirven para el anáĺisis léxico.
-}
module Token
( Token (..)
)where

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

-- | Función para que sea más sencillo debuggear.
instance Show Token where
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