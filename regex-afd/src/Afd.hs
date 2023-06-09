{- | Módulo para generar el AFD mínimo a partir de una Regex.

La manera en que se representa un AFD es con:

            ([(Int, Bool)], [(Int, Int, Char)])

La primera lista corresponde a la lista de estados, (con un índice
para saber si es un estado final).

La segunda lista corresponde a las aristas, tiene los estados que
conecta y el caracter as
-}
module Afd
( shift
, match
, afd
, markFirst
) where

import Regex
import Data.List (nub)
import qualified Data.HashMap.Strict as HM

-- | Crea el AFD de una Regex.
afd :: Regex -> ([(Int, Bool)], [(Int, Int, Char)])
afd regex = let (hm, aristas) = getAfd regex
            in (markFinals hm, aristas)

-- | Le agrega una marca inicial al Regex, para que pueda usarseen el algoritmo.
markFirst :: Regex -> Regex
markFirst = Sec (Ter True Epsil)

-- | Nos da el estado pozo de una expresión regular (la expresión sin marcas).
pozo :: Regex -> Regex
pozo = Sec (Ter False Epsil)

{- | Recibe una expresión regular marcada, un booleano, y un caracter.
Devuelve una expresión regular marcada, resultante de intentar avanzar las
marcas con el caracter.
El valor booleano es un parámetro para casos recursivos, indica si en una
concatenación, el valor inmediato anterior estuvo marcado.

Esta función es la que viene descrita en:

Fischer, S., Huch, F., & Wilke, T. (2010). A play on regular expressions:
functional pearl. ACM SIGPLAN International Conference on Functional Programming
-}
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

{- | Si la expresión regular es anulable.
Es decir si puede generar épsilon.
-}
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

-- | Si la expresión marcada está en un estado final.
final :: Regex -> Bool
final Null = False
final (Ter b _) = b
final (Dis p q) = final p || final q
final (Sec p q) = (final p && empty q) || final q
final (Mas m) = final m
final (Rep r) = final r
final (Int i) = final i

{- | Revisa si una expresión regular reconoce una cadena como parte
de su lenguaje.
Esta función no es necesaria parael proyecto.
-}
match :: Regex -> String -> Bool
match r [] = empty r
match r (c:cs) = final (foldl (shift False) (shift True r c) cs)

{- | Obtiene el alfabeto de una expreisón regular, el alfabeto consta
de todos los caracteres presentes en la expresión. Con duplicados.
-}
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

-- | Obtiene el alfabeto de una expresión regular, elimina los duplicados.
alphabet :: Regex -> String
alphabet = nub . getAlphabet

-- | Obtiene el AFD mínimo de una expresíon regular de entrada.
getAfd :: Regex -> (HM.HashMap Regex Int, [(Int, Int, Char)])
getAfd r = createAFD [markFirst r] (alphabet r) (alphabet r) 1 [] (HM.fromList [(markFirst r, 0), (pozo r, -1)])

{- | Crea el AFD de una expresión regular.
Para eso tiene genera las transiciones de un estado con todos los caracteres posibles.
Usa el algoritmo descrito en:

 Dick Grune, Kees van Reeuwijk, Henri E. Bal, Ceriel J.H. Jacobs, and Koen Lan-
gendoen. 2012. Modern Compiler Design (2nd. ed.). Springer Publishing Company,
Incorporated. (Pags. 74-83).

El primer parámetro es una lista de Regex con todas las regex a las que les falta generar sus transiciones.
El segundo parámetro es una lista de caracteres, de todos con los que falta generar una transición de la regex actual.
El tercer parámetro es el alfabeto de la regex. Sirve para resetear el parámetro anterior.
El cuarto parámetro es un contador, que tiene el número del siguiente estado nuevo a generar.
El quinto arámetro es una lista de las aristas, se aumenta cada que se genera una transición de un estado a otro.
El sexto parámetro es un diccionario que mapea los estados generados (Regex con marcas) a un entero (su índice).

Devuelve una tupla que contiene el diccionario de estados y la lista de aristas.
-}
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

{- | Recibe un diccionario con todos los estados del AFD, convierte
el diccionario en una lista, de sus índices, y un valor booleano que indica
si son estados finales.
-}
markFinals :: HM.HashMap Regex Int -> [(Int, Bool)]
markFinals = HM.foldrWithKey isFinal []
    where
        isFinal _ (-1) acc = acc
        isFinal regex indice acc = (indice, final regex):acc