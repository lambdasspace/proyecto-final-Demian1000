module Afd
( shift
, match
, afd
, markFirst
) where

import Regex
import Data.List (nub)
import qualified Data.HashMap.Strict as HM


markFirst :: Regex -> Regex
markFirst = Sec (Ter True Epsil)

pozo :: Regex -> Regex
pozo = Sec (Ter False Epsil)

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

-- Si es un estado final
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

getAlphabet :: Regex -> String
getAlphabet Null = ""
getAlphabet (Ter _ x) =
    case x of
        (Rango r) -> r
        (Negac n) -> [n]
        (Simbo s) -> [s]
        Punto -> ""
        Epsil -> ""
getAlphabet (Dis x y) = getAlphabet x ++ getAlphabet y
getAlphabet (Sec x y) = getAlphabet x ++ getAlphabet y
getAlphabet (Mas x) = getAlphabet x
getAlphabet (Rep x) = getAlphabet x
getAlphabet (Int x) = getAlphabet x

alphabet :: Regex -> String
alphabet = nub . getAlphabet

afd :: Regex -> ([(Int, Bool)], [(Int, Int, Char)])
afd regex = let (hm, aristas) = getAfd regex
            in (markFinals hm, aristas)

getAfd :: Regex -> (HM.HashMap Regex Int, [(Int, Int, Char)])
getAfd r = createAFD [markFirst r] (alphabet r) (alphabet r) 1 [] (HM.fromList [(markFirst r, 0), (pozo r, -1)])

createAFD :: [Regex] -> String -> String -> Int -> [(Int, Int, Char)] -> HM.HashMap Regex Int -> (HM.HashMap Regex Int, [(Int, Int, Char)])
createAFD [] _ _ _ aristas estados = (estados,aristas)
createAFD (_:rs) [] alfabeto contador aristas estados = createAFD rs alfabeto alfabeto contador aristas estados
createAFD (r:rs) (a:as) alfabeto contador aristas estados =
    let nuevoEstado = shift False r a
    in case HM.lookup r estados of
        Nothing -> (HM.fromList [], [(0,0,'-')]) -- Este caso no debería darse
        (Just num1) -> case HM.lookup nuevoEstado estados of
            Nothing -> createAFD ((r:rs) ++ [nuevoEstado]) as alfabeto (contador+1) ((num1, contador, a):aristas) (HM.insert nuevoEstado contador estados)
            (Just (-1)) -> createAFD (r:rs) as alfabeto contador aristas estados
            (Just num2) -> createAFD (r:rs) as alfabeto contador ((num1, num2, a):aristas) estados


markFinals :: HM.HashMap Regex Int -> [(Int, Bool)]
markFinals = HM.foldrWithKey isFinal []
    where
        isFinal _ (-1) acc = acc
        isFinal regex indice acc = (indice, final regex):acc