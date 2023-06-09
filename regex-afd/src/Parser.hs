{- | Módulo para generar la Regex a partir de una PreRegex -}
module Parser
    ( Regex(..)
    , parse
    ) where

import PreRegex
import Token
import Regex

-- | Construye el Regex a partir de un PreRegex.
parse :: PreRegex -> Regex
parse = construyeRegex Null 

-- | Función recursiva para crear un Regex.
construyeRegex :: Regex -> PreRegex -> Regex
construyeRegex acc (SUBREGEX []) = acc
construyeRegex Null (SUBREGEX (x:xs)) =
    case x of
        (SUBREGEX _) -> construyeRegex (construyeRegex Null x) (SUBREGEX xs)
        (TERMINAL t) -> construyeRegex (Ter False t) (SUBREGEX xs)
        _ -> Null
construyeRegex acc (SUBREGEX (x:xs)) =
    case x of
        (OPERADOR INTERROGACION) -> construyeRegex (Int acc) (SUBREGEX xs)
        (OPERADOR DISYUNCION) -> Dis acc (construyeRegex Null (SUBREGEX xs))
        (OPERADOR ESTRELLA) -> construyeRegex (Rep acc) (SUBREGEX xs)
        (OPERADOR MAS) -> construyeRegex (Mas acc) (SUBREGEX xs)
        (SUBREGEX _) -> construyeRegex (Sec acc (construyeRegex Null x)) (SUBREGEX xs)
        (TERMINAL t) -> construyeRegex (Sec acc (Ter False t)) (SUBREGEX xs)
        _ -> Null
construyeRegex _ _ = Null