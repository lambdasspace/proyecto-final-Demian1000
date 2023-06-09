{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- | Tipo de dato para representar las expresiones regulares.

Esta representación se basa en la presentada en el artículo:

Fischer, S., Huch, F., & Wilke, T. (2010). A play on regular expressions:
functional pearl. ACM SIGPLAN International Conference on Functional Programming
-}
module Regex
( Regex(..)
, Terminal(..)
) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | Tipo de expresiones regulares.
data Regex
    = Null --Valor base para poder tener un acumulador inicial
    | Ter Bool Terminal
    | Dis Regex Regex -- |
    | Sec Regex Regex -- αβ
    | Mas Regex -- +
    | Rep Regex -- *
    | Int Regex -- ?
    deriving (Eq, Generic, Hashable)

-- | Tipo de dato para encapsular todo slos tipos de terminales.
data Terminal
    = Rango String
    | Negac Char
    | Simbo Char
    | Punto
    | Epsil
    deriving(Eq, Generic, Hashable)

-- | Forma de ver para debuggeo
instance Show Terminal where
    show :: Terminal -> String
    show (Rango r) = "[" ++ r ++ "]"
    show (Negac n) = "~" ++ [n]
    show (Simbo s) = [s]
    show Punto = "."
    show Epsil = "ε"

-- | Forma de ver para debugeo
instance Show Regex where
    show :: Regex -> String
    show Null = ""
    show (Ter b t) = (if b then "•" else "") ++ show t
    show (Dis a b) = "Dis (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Sec a b) = show a ++ show b
    show (Mas m) = "(Mas " ++ show m ++ ")"
    show (Rep m) = "(Rep " ++ show m ++ ")"
    show (Int m) = "(Int " ++ show m ++ ")"