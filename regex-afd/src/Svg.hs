{- | Módulo para generar la imagen en formato svg.

Provee las funciones necesarias para generar líneas rectas,
rectángulos y caracteres.

La imagen creada se va a colocar dentro de un cuadrante, cuya
unidad es un valor constante sc, todas las imágenes van a
estar escaladas por este valor.
-}
module Svg
( svgHeader
, svgEnder
, svgRectangle
, svgNum
, svgChar
, svgLine
) where

-- | Escala va a tener el svg.
sc :: Int
sc = 40

-- | Escala un valor.
scale :: Int -> Int
scale = (sc*)

-- | Escala un valor y lo mueve dentro de un margen.
scaleM :: Int -> Int
scaleM = (sc+).(sc*)

-- | Escalar un valor y dentro de un margen. (Para flotantes).
fscaleM :: Float -> Float
fscaleM = (fromIntegral sc+).(fromIntegral sc*)

-- | Escala un valor y lo desplaza para que quede en la mitad de un cuadrante.
scaleL :: Int -> Int
scaleL = (sc+div sc 2 +).(sc*)

-- | Crea La líneas iniciales de la imagen svg.
svgHeader :: Int -> Int -> String
svgHeader width height =
    "<?xml version='1.0' encoding='UTF-8' ?>\n" ++ 
    "<svg width='" ++ show (scaleM width) ++ "' height='" ++
    show (scaleM height) ++ "'>\n  <g>\n" ++
    "    <rect x='" ++ show 0 ++ "' y='" ++ show 0 ++ "' " ++
    "width='" ++ show (scaleM width) ++ "' height='" ++ show (scaleM height) ++
    "' style='stroke-width:3; stroke:black;fill:ghostwhite'/>\n"

-- | La línea final de la imagen svg.
svgEnder :: String
svgEnder = "  </g>\n</svg>"

{- | Dibuja un rectángulo y dibuja su id en el centro.

Por orden de aparición los parámetros son: x y xx yy id b.

x es la coordenada en x de la esquina superior izquierda.
y es la coordenada en y de la equina superior izquierda.
xx es la anchura del rectángulo.
yy es la altura del rectángulo.
id es el id del rectángulo.
b es un valor booleano que determina si se va a colorear el rectángulo.
-}
svgRectangle :: Int -> Int -> Int -> Int -> Int -> Bool -> String
svgRectangle x y width height numero b =
    "    <rect x='" ++ show (scaleM x) ++ "' y='" ++ show (scaleM y) ++ "' " ++
    "width='" ++ show (scale width) ++ "' height='" ++ show (scale height) ++
    "' style='stroke-width:3; stroke:black;fill:" ++ (if b then "khaki" else "ghostwhite") ++
    "'/>\n" ++
    svgNum numero (fromIntegral x + fromIntegral width/2) (fromIntegral y + fromIntegral height/2)

-- | Dibuja un número en una coordenada.
svgNum :: Int -> Float -> Float -> String
svgNum i x y = "    <text x='" ++ show (fscaleM x) ++ "' y='" ++ show (fscaleM y) ++
            "' fill='black'>" ++ show i ++ "</text>\n"

-- | Dibuja un caracter en una coordenada.
svgChar :: Int -> Int -> Char -> String
svgChar x y c = "    <text x='" ++ show (scaleM x) ++ "' y='" ++ show (scaleM y) ++
             "' fill='black'>" ++ [c] ++ "</text>\n"

-- | Dibuja una línea recta que conecta las dos coordenadas de entrada.
svgLine :: ((Int, Int), (Int, Int)) -> String
svgLine ((x1, y1), (x2, y2)) =
    "    <line x1='" ++ show (scaleL x1) ++ "' y1='" ++ show (scaleL y1) ++ 
    "' x2='" ++ show (scaleL x2) ++ "' y2='" ++ show (scaleL y2) ++ 
    "' style='stroke:black;stroke-width:2' />\n"