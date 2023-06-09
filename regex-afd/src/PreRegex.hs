{-# LANGUAGE InstanceSigs #-}

{- | Tipo de dato para transicionar entre los Tokens y
la expresión regular.

Tipo para poder tener listas dentro de listas recursivamente.
-}
module PreRegex
( PreRegex (..)
, bind
)where

import Regex (Terminal(..))
import Token

-- | Tipo recursivo para poder tener listas de Tokens dentro
-- de listas de tokens, etc.
data PreRegex
    = OPERADOR Token
    | SUBREGEX [PreRegex]
    | TERMINAL Terminal

-- | Función para debuggeo.
instance Show PreRegex where
    show :: PreRegex -> String
    show (OPERADOR o) = show o
    show (SUBREGEX ls) = show ls
    show (TERMINAL t) = show t

-- | Une dos expresiones PreRegex en una sola.
bind :: PreRegex -> PreRegex -> PreRegex
bind (SUBREGEX s) (SUBREGEX z) = SUBREGEX $ s ++ z
bind (SUBREGEX s) o = SUBREGEX $ s ++ [o]
bind o (SUBREGEX z) = SUBREGEX $ o:z
bind o t = SUBREGEX [o, t]