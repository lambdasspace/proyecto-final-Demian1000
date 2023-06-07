module DrawAfd
( drawAfd
, nodos
)where

import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import Afd
import Svg

nodos (vertices, aristas) = nodePlacement vertices aristas

drawAfd :: ([(Int, Bool)], [(Int, Int, Char)]) -> String
drawAfd (vertices, aristas) =
    let nodos = nodePlacement vertices aristas
        toSvg num (x, y, xx, yy, b) acc = acc ++ rectangle x y xx yy num b
        contenido = HM.foldrWithKey toSvg "" nodos
        transiciones = drawEdges aristas nodos
    in header (getWidth nodos) (getHeight nodos) ++ transiciones ++ contenido ++ ender


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


edgeRouting :: [(Int, Int, Char)] -> HM.HashMap Int (Int, Int, Int, Int, Bool) -> Maybe [((Int, Int), (Int, Int), Char)]
edgeRouting aristas hm =
    mapM (\(a, b, c) -> do
        (x1, y1, _, _, _) <- HM.lookup a hm
        (x2, y2, _, _, _) <- HM.lookup b hm
        return ((x1, y1), (x2, y2), c)
    ) aristas


coordLabel :: [((Int, Int), (Int, Int), Char)] -> [(Int, Int, Char)]
coordLabel = foldr (\((_, w2), (w3, _), c) ls -> (w3, w2, c):ls) []


outEdges :: [((Int, Int), (Int, Int), Char)] -> [((Int, Int), (Int, Int))]
outEdges = foldr (\((w1, w2), (w3, _), _) ls -> ((w1, w2), (w3, w2)):ls) []


inEdges :: [((Int, Int), (Int, Int), Char)] -> [((Int, Int), (Int, Int))]
inEdges = foldr (\((_, w2), (w3, w4), _) ls -> ((w3, w2), (w3, w4)):ls) []


quitMaybe :: Maybe [((Int, Int), (Int, Int), Char)] -> [((Int, Int), (Int, Int), Char)]
quitMaybe Nothing = []
quitMaybe (Just hm) = hm

drawEdges :: [(Int, Int, Char)] -> HM.HashMap Int (Int, Int, Int, Int, Bool) -> String
drawEdges aristas hm =
    let mapEdges = edgeRouting aristas hm
        edges1 = quitMaybe mapEdges
        edges2 = shiftDownOut (sortBy (\((_,b1),(_,_),_) ((_,b2),(_,_),_) -> compare b1 b2) edges1) 0 0 []
        edges3 = shiftRightIn (sortBy (\((_,_),(c1,_),_) ((_,_),(c2,_),_) -> compare c1 c2) edges2) 0 0 []
        --edges4 = shiftRightOut (sortBy (\((a1,_),(_,_),_) ((a2,_),(_,_),_) -> compare a1 a2) edges3) 0 0 []
        --edges5 = shiftDownIn (sortBy (\((_,_),(_,d1),_) ((_,_),(_,d2),_) -> compare d1 d2) edges4) 0 0 []
        
        --out = shiftDownOut (sortBy (\((_,b1),(_,_)) ((_,b2),(_,_)) -> compare b1 b2) (outEdges mapEdges)) 0 0 []
        --in_ = shiftRightIn (sortBy (\((_,_),(c1,_)) ((_,_),(c2,_)) -> compare c1 c2) (inEdges mapEdges)) 0 0 []
        (edges6, edges7) = divideLoops edges3
        p0 = drawLoops edges7
        p1 = foldr (\(x, y, c) acc -> acc ++ svgChar x y c) "" (coordLabel edges6)
        p2 = foldr (\l acc -> acc ++ line l) "" (outEdges edges6)
        p3 = foldr (\l acc -> acc ++ line l) "" (inEdges edges6)
    in
        p0 ++ p1 ++ p2 ++ p3


divideLoops :: [((Int, Int), (Int, Int), Char)] -> ([((Int, Int), (Int, Int), Char)], [((Int, Int), (Int, Int), Char)])
divideLoops =
    foldr (\((a, b), (c, d), char) (acc1, acc2)
        -> if a==c || b==d
            then (acc1, ((a, b), (c, d), char):acc2)
            else (((a, b), (c, d), char):acc1, acc2)) ([], [])


drawLoops :: [((Int, Int), (Int, Int), Char)] -> String
drawLoops ls =
    let loops = foldr (\((a,b), (c,d), char) acc -> ((a,b),(a-1,d-1),char):((a-1,d-1),(c,d),char):acc) [] ls
        labels = foldr (\((a,_), (_,d), char) acc -> svgChar (a-1) d char ++ acc) "" ls
        p0 = foldr (\l acc -> acc ++ line l) "" (outEdges loops)
        p1 = foldr (\l acc -> acc ++ line l) "" (inEdges loops)
    in
        p0 ++ p1 ++ labels


-- Se presupone que vienen ordenados
shiftDownOut :: [((Int, Int), (Int, Int), Char)] -> Int -> Int -> [((Int, Int), (Int, Int), Char)] -> [((Int, Int), (Int, Int), Char)]
shiftDownOut [] _ _ acc = acc
shiftDownOut (((a,b), (c,d), char):cs) e contador acc =
    if b == e then
        shiftDownOut cs b (contador+1) (((a,b+contador), (c,d), char):acc)
    else
        shiftDownOut (((a,b), (c,d), char):cs) b 0 acc

-- shiftRightOut :: [((Int, Int), (Int, Int), Char)] -> Int -> Int -> [((Int, Int), (Int, Int), Char)] -> [((Int, Int), (Int, Int), Char)]
-- shiftRightOut [] _ _ acc = acc
-- shiftRightOut (((a,b), (c,d), char):cs) e contador acc =
--     if a == e then
--         shiftRightOut cs a (contador+1) (((a+contador,b), (c,d), char):acc)
--     else
--         shiftRightOut (((a,b), (c,d), char):cs) a 0 acc

-- Se presupone que vienen ordenados
shiftRightIn :: [((Int, Int), (Int, Int), Char)] -> Int -> Int -> [((Int, Int), (Int, Int), Char)] -> [((Int, Int), (Int, Int), Char)]
shiftRightIn [] _ _ acc = acc
shiftRightIn (((a,b), (c,d), char):cs) e contador acc =
    if c == e then
        shiftRightIn cs c (contador+1) (((a,b), (c+contador,d), char):acc)
    else
        shiftRightIn (((a,b), (c,d), char):cs) c 0 acc

-- shiftDownIn :: [((Int, Int), (Int, Int), Char)] -> Int -> Int -> [((Int, Int), (Int, Int), Char)] -> [((Int, Int), (Int, Int), Char)]
-- shiftDownIn [] _ _ acc = acc
-- shiftDownIn (((a,b), (c,d), char):cs) e contador acc =
--     if d == e then
--         shiftDownIn cs d (contador+1) (((a,b), (c,d+contador), char):acc)
--     else
--         shiftDownIn (((a,b), (c,d), char):cs) d 0 acc

--portAssignment :: (Int -> Int)

entrantes :: Int -> [(Int, Int, Char)] -> Int
entrantes estado aristas = length $ filter (\(_, b, _) -> estado == b) aristas

salientes :: Int -> [(Int, Int, Char)] -> Int
salientes estado aristas = length $ filter (\(a, _, _) -> estado == a) aristas