module Afd
( shift
, markFirst
, match
) where

import Regex

markFirst :: Regex -> Regex
markFirst Null = Null
markFirst (Ter _ Epsil) = Ter False Epsil
markFirst (Ter _ t) = Ter True t
markFirst (Dis p q) = Dis (markFirst p) (markFirst q)
markFirst (Sec p q) = Sec (markFirst p) (if empty p then markFirst q else q)
markFirst (Mas m) = Mas (markFirst m)
markFirst (Rep r) = Rep (markFirst r)
markFirst (Int i) = Int (markFirst i)

-- Se puede optimizar con recursión de cola
shift :: Bool -> Regex -> Char -> Regex
shift _ Null _ = Null
shift b (Ter _ t) c = case t of
    (Rango r) -> Ter (b && elem c r) (Rango r)
    (Negac n) -> Ter (b && c /= n) (Negac n)
    (Simbo s) -> Ter (b && c == s) (Simbo s)
    Punto -> Ter b Punto
    Epsil -> Ter False Epsil
shift b (Dis p q) c = Dis (shift b p c) (shift b q c)
shift b (Sec p q) c = Sec (shift b p c) (shift ((b && empty p) || final p) q c)
shift b (Mas p) c = Mas (shift (b || final p) p c)
shift b (Rep p) c = Rep (shift (b || final p) p c)
shift b (Int p) c = Int (shift b p c)

-- Si la expresión es anulable
empty :: Regex -> Bool
empty Null = True
empty (Ter _ t) = case t of
    (Rango r) -> r == ""
    (Negac _) -> False
    (Simbo _) -> False
    Punto -> False
    Epsil -> True
empty (Dis p q) = empty p || empty q
empty (Sec p q) = empty p && empty q
empty (Mas _) = False
empty (Rep _) = True
empty (Int _) = True

-- Si la expresión es anulable
final :: Regex -> Bool
final Null = False
final (Ter b _) = b
final (Dis p q) = final p || final q
final (Sec p q) = (final p && empty q) || final q
final (Mas m) = final m
final (Rep r) = final r
final (Int i) = final i


match :: Regex -> String -> Bool
match r [] = empty r
match r (c:cs) = final (foldl (shift False) (shift True r c) cs)