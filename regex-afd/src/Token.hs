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