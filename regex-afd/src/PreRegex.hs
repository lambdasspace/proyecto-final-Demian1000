{-# LANGUAGE InstanceSigs #-}

module PreRegex
( PreRegex (..)
, bind
)where

import Regex (Terminal(..))
import Token

-- Tipo recursivo para poder tener listas de Tokens dentro de listas de tokens, etc.
data PreRegex
    = OPERADOR Token
    | SUBREGEX [PreRegex]
    | TERMINAL Terminal

-- forma d ever para debuggeo
instance Show PreRegex where
    show :: PreRegex -> String
    show (OPERADOR o) = show o
    show (SUBREGEX ls) = show ls
    show (TERMINAL t) = show t

bind :: PreRegex -> PreRegex -> PreRegex
bind (SUBREGEX s) (SUBREGEX z) = SUBREGEX $ s ++ z
bind (SUBREGEX s) o = SUBREGEX $ s ++ [o]
bind o (SUBREGEX z) = SUBREGEX $ o:z
bind o t = SUBREGEX [o, t]