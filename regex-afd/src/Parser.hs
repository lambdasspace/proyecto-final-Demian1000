module Parser
    ( Regex(..)
    , parse
    ) where

import PreParser (PreRegex(..), Terminal)
import Lexer (Token(..))

data Regex
    = Null --Valor base para poder tener un acumulador inicial
    | Ter Terminal
    | Dis Regex Regex -- |
    | Sec Regex Regex -- αβ
    | Mas Regex -- +
    | Rep Regex -- *
    | Int Regex -- ?
    deriving (Eq, Show)

parse :: PreRegex -> Regex
parse = construyeRegex Null 

construyeRegex :: Regex -> PreRegex -> Regex
-- construyeRegex Null (Terminal t) =
-- construyeRegex acc (Terminal t) = -- Caso sus
-- construyeRegex Null (OPERADOR o) = Null
-- construyeRegex acc (OPERADOR o)
construyeRegex acc (SUBREGEX []) = acc
construyeRegex Null (SUBREGEX (x:xs)) =
    case x of
        (SUBREGEX _) -> construyeRegex (construyeRegex Null x) (SUBREGEX xs)
        (TERMINAL t) -> construyeRegex (Ter t) (SUBREGEX xs)
        _ -> Null
construyeRegex acc (SUBREGEX (x:xs)) =
    case x of
        (OPERADOR INTERROGACION) -> construyeRegex (Int acc) (SUBREGEX xs)
        (OPERADOR DISYUNCION) -> Dis acc (construyeRegex Null (SUBREGEX xs))
        (OPERADOR ESTRELLA) -> construyeRegex (Rep acc) (SUBREGEX xs)
        (OPERADOR MAS) -> construyeRegex (Mas acc) (SUBREGEX xs)
        (SUBREGEX _) -> construyeRegex (Sec acc (construyeRegex Null x)) (SUBREGEX xs)
        (TERMINAL t) -> construyeRegex (Sec acc (Ter t)) (SUBREGEX xs)
        _ -> Null
construyeRegex _ _ = Null