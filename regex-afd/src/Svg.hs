module Svg
( svgHeader
, svgEnder
, svgRectangle
, svgNum
, svgChar
, svgLine
) where


sc :: Int
sc = 40

scale :: Int -> Int
scale = (sc*)

scaleM :: Int -> Int
scaleM = (sc+).(sc*)

fscaleM :: Float -> Float
fscaleM = (fromIntegral sc+).(fromIntegral sc*)

scaleL :: Int -> Int
scaleL = (sc+div sc 2 +).(sc*)

svgHeader :: Int -> Int -> String
svgHeader width height =
    "<?xml version='1.0' encoding='UTF-8' ?>\n" ++ 
    "<svg width='" ++ show (scaleM width) ++ "' height='" ++
    show (scaleM height) ++ "'>\n  <g>\n" ++
    "    <rect x='" ++ show 0 ++ "' y='" ++ show 0 ++ "' " ++
    "width='" ++ show (scaleM width) ++ "' height='" ++ show (scaleM height) ++
    "' style='stroke-width:3; stroke:black;fill:ghostwhite'/>\n"

svgEnder :: String
svgEnder = "  </g>\n</svg>"

svgRectangle :: Int -> Int -> Int -> Int -> Int -> Bool -> String
svgRectangle x y width height numero b =
    "    <rect x='" ++ show (scaleM x) ++ "' y='" ++ show (scaleM y) ++ "' " ++
    "width='" ++ show (scale width) ++ "' height='" ++ show (scale height) ++
    "' style='stroke-width:3; stroke:black;fill:" ++ (if b then "khaki" else "ghostwhite") ++
    "'/>\n" ++
    svgNum numero (fromIntegral x + fromIntegral width/2) (fromIntegral y + fromIntegral height/2)

svgNum :: Int -> Float -> Float -> String
svgNum i x y = "    <text x='" ++ show (fscaleM x) ++ "' y='" ++ show (fscaleM y) ++
            "' fill='black'>" ++ show i ++ "</text>\n"


svgChar :: Int -> Int -> Char -> String
svgChar x y c = "    <text x='" ++ show (scaleM x) ++ "' y='" ++ show (scaleM y) ++
             "' fill='black'>" ++ [c] ++ "</text>\n"

svgLine :: ((Int, Int), (Int, Int)) -> String
svgLine ((x1, y1), (x2, y2)) =
    "    <line x1='" ++ show (scaleL x1) ++ "' y1='" ++ show (scaleL y1) ++ 
    "' x2='" ++ show (scaleL x2) ++ "' y2='" ++ show (scaleL y2) ++ 
    "' style='stroke:black;stroke-width:2' />\n"