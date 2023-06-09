{- | Módulo para dibujar un AFD en un a imagen svg.

Utiliza el método de tres fases para graficación ortogonal en gráficas
descrito en:

Biedl, Therese & Madden, Brendan & Tollis, Ioannis. (2000). The Three-Phase Method:
A Unified Approach to Orthogonal Graph Drawing.. Int. J. Comput. Geometry Appl..
10. 553-580. 10.1142/S0218195900000310
-}
module DrawAfd
( drawAfd
, states
)where

import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import Svg

{- | Dado un AFD representado como una lista de estados y una lista de aristas,
Obtiene la cadena correspondiente al dibujo svg del autómata.
-}
drawAfd :: ([(Int, Bool)], [(Int, Int, Char)]) -> String
drawAfd (vertices, aristas) =
    let nodos = nodePlacement vertices aristas
        toSvg num (x, y, xx, yy, b) acc = acc ++ svgRectangle x y xx yy num b
        contenido = HM.foldrWithKey toSvg "" nodos
        transiciones = drawEdges aristas nodos
    in svgHeader (getWidth nodos) (getHeight nodos) ++ transiciones ++ contenido ++ svgEnder

{- | Dado un diccionario que mapea los índices de los estados a sus coordenadas, dimensiones
y un valor booleano sque indica si es final.
Obtiene la anchura que va a necesitar la imagen svg para contener el AFD.
-}
getWidth :: HM.HashMap Int (Int, Int, Int, Int, Bool) -> Int
getWidth hs = 1 + HM.foldrWithKey (\_ (x, _, xx, _, _) acc -> max (x+xx) acc) 0 hs

{- | Dado un diccionario que mapea los índices de los estados a sus coordenadas, dimensiones
y un valor booleano sque indica si es final.
Obtiene la altura que va a necesitar la imagen svg para contener el AFD.
-}
getHeight :: HM.HashMap Int (Int, Int, Int, Int, Bool) -> Int
getHeight hs = 1 + HM.foldrWithKey (\_ (_, y, _, yy, _) acc -> max (y+yy) acc) 0 hs

{- | Dado un AFD representado como una lista de estados y una lista de aristas,
le asigna una coordenada en el eje x (su índice), y una coordenada en el eje y (el orden en el que vienen);
y los guarda en un diccionario.
-}
nodePlacement :: [(Int, Bool)] -> [(Int, Int, Char)] -> HM.HashMap Int (Int, Int, Int, Int, Bool)
nodePlacement ls aristas =
    let coords = zipWith (\(x, b) y -> (x, x, y, -1, -1, b)) ls [0..(length ls)]
        compareByX (_, x1, _, _, _, _) (_, x2, _, _, _, _) = compare x1 x2
        e1 = expandeSalidas coords 0 [] aristas
        e2 = expandeEntradas (sortBy compareByX e1) 0 [] aristas
    in foldr (\(i, x, y, xx, yy, b) hm -> HM.insert i (x,y,xx,yy,b) hm) HM.empty e2

-- | Función para debuggeo, no es necesaria para el proyecto.
states :: ([(Int, Bool)], [(Int, Int, Char)]) -> HM.HashMap Int (Int, Int, Int, Int, Bool)
states (vertices, aristas) = nodePlacement vertices aristas


{- | Recibe una lsita con las coordenadas de los vértices, y calcula el desplazamiento que tiene que tener
sus coordenadas por la creación de los puertos en los vértices, y calcula las dimensiones que necesita el vértice

Eje Y.

El primer parámetro es la lista con las coordenadas, dimensiones y valor de final delos vértices (aunque las dimensiones
que tiene de entrada se ignoran).
El segundo parámetro es un acumulador para contar el desplazamiento que tiene que tener el vértice en el eje y.
El tercer parámetro es un acumulador para hacer recursión de cola.
El último parámetro es la lista de aristas.
-}
expandeSalidas :: [(Int, Int, Int, Int, Int, Bool)] -> Int -> [(Int, Int, Int, Int, Int, Bool)] -> [(Int, Int, Char)]
                    -> [(Int, Int, Int, Int, Int, Bool)]
expandeSalidas [] _ acc _ = acc
expandeSalidas ((i, x, y, xx, _, b):xs) desplazamiento acc aristas =
    let salidas = max 1 (salientes i aristas)
    in expandeSalidas xs (desplazamiento + salidas) ((i, x, y + desplazamiento, xx, salidas, b):acc) aristas

{- | Recibe una lsita con las coordenadas de los vértices, y calcula el desplazamiento que tiene que tener
sus coordenadas por la creación de los puertos en los vértices, y calcula las dimensiones que necesita el vértice.

Eje X.

El primer parámetro es la lista con las coordenadas, dimensiones y valor de final delos vértices (aunque las dimensiones
que tiene de entrada se ignoran).
El segundo parámetro es un acumulador para contar el desplazamiento que tiene que tener el vértice en el eje x.
El tercer parámetro es un acumulador para hacer recursión de cola.
El último parámetro es la lista de aristas.
-}
expandeEntradas :: [(Int, Int, Int, Int, Int, Bool)] -> Int -> [(Int, Int, Int, Int, Int, Bool)] -> [(Int, Int, Char)]
                    -> [(Int, Int, Int, Int, Int, Bool)]
expandeEntradas [] _ acc _ = acc
expandeEntradas ((i, x, y, _, yy, b):xs) desplazamiento acc aristas =
    let entradas = max 1 (entrantes i aristas)
    in expandeEntradas xs (desplazamiento + entradas) ((i, x + desplazamiento, y, entradas, yy, b):acc) aristas

{- | Dada una lista de aristas y el diccionario con las coordenadas de los estados,
obtiene una lista de aristas, pero con las coordenadas de sus extremos, en vez de los
estados que conecta.
El valor de retorno debe ser Maybe, pero es necesario.
-}
edgeRouting :: [(Int, Int, Char)] -> HM.HashMap Int (Int, Int, Int, Int, Bool) -> Maybe [((Int, Int), (Int, Int), Char, Bool)]
edgeRouting aristas hm =
    mapM (\(a, b, c) -> do
        (x1, y1, _, _, _) <- HM.lookup a hm
        (x2, y2, _, _, _) <- HM.lookup b hm
        return ((x1, y1), (x2, y2), c, a == b)
    ) aristas

-- | Determina la coordenada que debe tener la etiqueta de una arista.
coordLabel :: [((Int, Int), (Int, Int), Char)] -> [(Int, Int, Char)]
coordLabel = foldr (\((_, w2), (w3, _), c) ls -> (w3, w2, c):ls) []

{- | Dada una lista de aristas, parte la arista, y solo regresa la parte de la arista
que sale en un vértice. Estas aristas siempre son horizontales.
-}
outEdges :: [((Int, Int), (Int, Int), Char)] -> [((Int, Int), (Int, Int))]
outEdges = foldr (\((w1, w2), (w3, _), _) ls -> ((w1, w2), (w3, w2)):ls) []

{- | Dada una lista de aristas, parte la arista, y solo regresa la parte de la arista
que incide en un vértice. Estas aristas siempre son verticales.
-}
inEdges :: [((Int, Int), (Int, Int), Char)] -> [((Int, Int), (Int, Int))]
inEdges = foldr (\((_, w2), (w3, w4), _) ls -> ((w3, w2), (w3, w4)):ls) []

{- | Desencapsula la lista de aristas quitandole el valor maybe,
el maybe se obtiene porque se buscan sus coordenadas en un diccionario,
pero el programa mismo garantiza que siempre devuelva valores validos
-}
quitMaybe :: Maybe [((Int, Int), (Int, Int), Char, Bool)] -> [((Int, Int), (Int, Int), Char, Bool)]
quitMaybe Nothing = []
quitMaybe (Just hm) = hm

{- | Recibe la lista de aristas, y el diccionario con las coordenadas de los estados,
y devuelve la cadena que representa el dibujo svg de las aristas.
-}
drawEdges :: [(Int, Int, Char)] -> HM.HashMap Int (Int, Int, Int, Int, Bool) -> String
drawEdges aristas hm =
    let mapEdges = edgeRouting aristas hm
        edges1 = quitMaybe mapEdges
        edges2 = shiftDownOut (sortBy (\((_,b1),(_,_),_, _) ((_,b2),(_,_),_, _) -> compare b1 b2) edges1) 0 0 []
        edges3 = shiftRightIn (sortBy (\((_,_),(c1,_),_, _) ((_,_),(c2,_),_, _) -> compare c1 c2) edges2) 0 0 []
        (edges6, edges7) = divideLoops edges3
        p0 = drawLoops edges7
        p1 = foldr (\(x, y, c) acc -> acc ++ svgChar x y c) "" (coordLabel edges6)
        p2 = foldr (\l acc -> acc ++ svgLine l) "" (outEdges edges6)
        p3 = foldr (\l acc -> acc ++ svgLine l) "" (inEdges edges6)
    in
        p0 ++ p1 ++ p2 ++ p3

{- | Separa a las aristas que son lazos de las que no, y elimina el valor booleano que indica si son lazos.

En la salida, la primera lista corresponde a las aristas que no son lazos, y la segunda a las que sí son lazos.
-}
divideLoops :: [((Int, Int), (Int, Int), Char, Bool)] -> ([((Int, Int), (Int, Int), Char)], [((Int, Int), (Int, Int), Char)])
divideLoops = foldr (\(ab, cd, char, bool) (acc1, acc2) ->
    if bool
        then (acc1, (ab, cd, char):acc2)
        else ((ab, cd, char):acc1, acc2)) ([], [])

-- | Dibuja las aristas que son lazos, para ello las divide en dos aristas diferentes.
drawLoops :: [((Int, Int), (Int, Int), Char)] -> String
drawLoops ls =
    let loops = foldr (\((a,b), (c,d), char) acc -> ((a,b),(a-1,d-1),char):((a-1,d-1),(c,d),char):acc) [] ls
        labels = foldr (\((a,_), (_,d), char) acc -> svgChar (a-1) d char ++ acc) "" ls
        p0 = foldr (\l acc -> acc ++ svgLine l) "" (outEdges loops)
        p1 = foldr (\l acc -> acc ++ svgLine l) "" (inEdges loops)
    in
        p0 ++ p1 ++ labels

{- | Modifica las coordenadas de las aristas agregando los desplazamientos (renglones) 
necesarios para que cada vértice tenga todos sus puertos de salida.

Se presupone que las aristas vienen ordenadas respecto al eje y del vértice del que salen.
-}
shiftDownOut :: [((Int, Int), (Int, Int), Char, Bool)] -> Int -> Int -> [((Int, Int), (Int, Int), Char, Bool)]
    -> [((Int, Int), (Int, Int), Char, Bool)]
shiftDownOut [] _ _ acc = acc
shiftDownOut (((a,b), (c,d), char, bool):cs) e contador acc =
    if b == e then
        shiftDownOut cs b (contador+1) (((a,b+contador), (c,d), char, bool):acc)
    else
        shiftDownOut (((a,b), (c,d), char, bool):cs) b 0 acc

{- | Modifica las coordenadas de las aristas agregando los desplazamientos (columnas) 
necesarias para que cada vértice tenga todos sus puertos de entrada.

Se presupone que las aristas vienen ordenadas respecto al eje x del vértice en el que inciden.
-}
shiftRightIn :: [((Int, Int), (Int, Int), Char, Bool)] -> Int -> Int -> [((Int, Int), (Int, Int), Char, Bool)]
    -> [((Int, Int), (Int, Int), Char, Bool)]
shiftRightIn [] _ _ acc = acc
shiftRightIn (((a,b), (c,d), char, bool):cs) e contador acc =
    if c == e then
        shiftRightIn cs c (contador+1) (((a,b), (c+contador,d), char, bool):acc)
    else
        shiftRightIn (((a,b), (c,d), char, bool):cs) c 0 acc

-- | Calcula la cantidad de aristas entrantes de un estado.
entrantes :: Int -> [(Int, Int, Char)] -> Int
entrantes estado aristas = length $ filter (\(_, b, _) -> estado == b) aristas

-- | Calcula la candidad de aristas salientes de un estado.
salientes :: Int -> [(Int, Int, Char)] -> Int
salientes estado aristas = length $ filter (\(a, _, _) -> estado == a) aristas