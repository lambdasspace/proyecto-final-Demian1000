module DrawAfd
( drawAfd
, states
)where

import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import Svg


drawAfd :: ([(Int, Bool)], [(Int, Int, Char)]) -> String
drawAfd (vertices, aristas) =
    let nodos = nodePlacement vertices aristas
        toSvg num (x, y, xx, yy, b) acc = acc ++ svgRectangle x y xx yy num b
        contenido = HM.foldrWithKey toSvg "" nodos
        transiciones = drawEdges aristas nodos
    in svgHeader (getWidth nodos) (getHeight nodos) ++ transiciones ++ contenido ++ svgEnder


getWidth :: HM.HashMap Int (Int, Int, Int, Int, Bool) -> Int
getWidth hs = 1 + HM.foldrWithKey (\_ (x, _, xx, _, _) acc -> max (x+xx) acc) 0 hs

getHeight :: HM.HashMap Int (Int, Int, Int, Int, Bool) -> Int
getHeight hs = 1 + HM.foldrWithKey (\_ (_, y, _, yy, _) acc -> max (y+yy) acc) 0 hs


nodePlacement :: [(Int, Bool)] -> [(Int, Int, Char)] -> HM.HashMap Int (Int, Int, Int, Int, Bool)
nodePlacement ls aristas =
    let coords = zipWith (\(x, b) y -> (x, x, y, -1, -1, b)) ls [0..(length ls)]
        compareByX (_, x1, _, _, _, _) (_, x2, _, _, _, _) = compare x1 x2
        e1 = expandeSalidas coords 0 [] aristas
        e2 = expandeEntradas (sortBy compareByX e1) 0 [] aristas
    in foldr (\(i, x, y, xx, yy, b) hm -> HM.insert i (x,y,xx,yy,b) hm) HM.empty e2

-- Para debuggeo
states :: ([(Int, Bool)], [(Int, Int, Char)]) -> HM.HashMap Int (Int, Int, Int, Int, Bool)
states (vertices, aristas) = nodePlacement vertices aristas


-- Recursión de cola
-- Primero
-- Eje y
expandeSalidas :: [(Int, Int, Int, Int, Int, Bool)] -> Int -> [(Int, Int, Int, Int, Int, Bool)] -> [(Int, Int, Char)]
                    -> [(Int, Int, Int, Int, Int, Bool)]
expandeSalidas [] _ acc _ = acc
expandeSalidas ((i, x, y, xx, _, b):xs) desplazamiento acc aristas =
    let salidas = max 1 (salientes i aristas)
    in expandeSalidas xs (desplazamiento + salidas) ((i, x, y + desplazamiento, xx, salidas, b):acc) aristas


expandeEntradas :: [(Int, Int, Int, Int, Int, Bool)] -> Int -> [(Int, Int, Int, Int, Int, Bool)] -> [(Int, Int, Char)]
                    -> [(Int, Int, Int, Int, Int, Bool)]
expandeEntradas [] _ acc _ = acc
expandeEntradas ((i, x, y, _, yy, b):xs) desplazamiento acc aristas =
    let entradas = max 1 (entrantes i aristas)
    in expandeEntradas xs (desplazamiento + entradas) ((i, x + desplazamiento, y, entradas, yy, b):acc) aristas


edgeRouting :: [(Int, Int, Char)] -> HM.HashMap Int (Int, Int, Int, Int, Bool) -> Maybe [((Int, Int), (Int, Int), Char, Bool)]
edgeRouting aristas hm =
    mapM (\(a, b, c) -> do
        (x1, y1, _, _, _) <- HM.lookup a hm
        (x2, y2, _, _, _) <- HM.lookup b hm
        return ((x1, y1), (x2, y2), c, a == b)
    ) aristas


coordLabel :: [((Int, Int), (Int, Int), Char)] -> [(Int, Int, Char)]
coordLabel = foldr (\((_, w2), (w3, _), c) ls -> (w3, w2, c):ls) []


outEdges :: [((Int, Int), (Int, Int), Char)] -> [((Int, Int), (Int, Int))]
outEdges = foldr (\((w1, w2), (w3, _), _) ls -> ((w1, w2), (w3, w2)):ls) []


inEdges :: [((Int, Int), (Int, Int), Char)] -> [((Int, Int), (Int, Int))]
inEdges = foldr (\((_, w2), (w3, w4), _) ls -> ((w3, w2), (w3, w4)):ls) []


quitMaybe :: Maybe [((Int, Int), (Int, Int), Char, Bool)] -> [((Int, Int), (Int, Int), Char, Bool)]
quitMaybe Nothing = []
quitMaybe (Just hm) = hm

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


divideLoops :: [((Int, Int), (Int, Int), Char, Bool)] -> ([((Int, Int), (Int, Int), Char)], [((Int, Int), (Int, Int), Char)])
divideLoops = foldr (\(ab, cd, char, bool) (acc1, acc2) ->
    if bool
        then (acc1, (ab, cd, char):acc2)
        else ((ab, cd, char):acc1, acc2)) ([], [])


drawLoops :: [((Int, Int), (Int, Int), Char)] -> String
drawLoops ls =
    let loops = foldr (\((a,b), (c,d), char) acc -> ((a,b),(a-1,d-1),char):((a-1,d-1),(c,d),char):acc) [] ls
        labels = foldr (\((a,_), (_,d), char) acc -> svgChar (a-1) d char ++ acc) "" ls
        p0 = foldr (\l acc -> acc ++ svgLine l) "" (outEdges loops)
        p1 = foldr (\l acc -> acc ++ svgLine l) "" (inEdges loops)
    in
        p0 ++ p1 ++ labels


-- Se presupone que vienen ordenados
shiftDownOut :: [((Int, Int), (Int, Int), Char, Bool)] -> Int -> Int -> [((Int, Int), (Int, Int), Char, Bool)]
    -> [((Int, Int), (Int, Int), Char, Bool)]
shiftDownOut [] _ _ acc = acc
shiftDownOut (((a,b), (c,d), char, bool):cs) e contador acc =
    if b == e then
        shiftDownOut cs b (contador+1) (((a,b+contador), (c,d), char, bool):acc)
    else
        shiftDownOut (((a,b), (c,d), char, bool):cs) b 0 acc


-- Se presupone que vienen ordenados
shiftRightIn :: [((Int, Int), (Int, Int), Char, Bool)] -> Int -> Int -> [((Int, Int), (Int, Int), Char, Bool)]
    -> [((Int, Int), (Int, Int), Char, Bool)]
shiftRightIn [] _ _ acc = acc
shiftRightIn (((a,b), (c,d), char, bool):cs) e contador acc =
    if c == e then
        shiftRightIn cs c (contador+1) (((a,b), (c+contador,d), char, bool):acc)
    else
        shiftRightIn (((a,b), (c,d), char, bool):cs) c 0 acc


entrantes :: Int -> [(Int, Int, Char)] -> Int
entrantes estado aristas = length $ filter (\(_, b, _) -> estado == b) aristas

salientes :: Int -> [(Int, Int, Char)] -> Int
salientes estado aristas = length $ filter (\(a, _, _) -> estado == a) aristas