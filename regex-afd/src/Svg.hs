module Svg
( header
, ender
, rectangle
, num
, svgChar
, line
) where

scale :: Int -> Int
scale = (40*)

scaleM :: Int -> Int
scaleM = (40+).(40*)

fscaleM :: Float -> Float
fscaleM = (40+).(40*)

scaleL :: Int -> Int
scaleL = (60+).(40*)

header :: Int -> Int -> String
header width height =
    "<?xml version='1.0' encoding='UTF-8' ?>\n" ++ 
    "<svg width='" ++ show (scaleM width) ++ "' height='" ++
    show (scaleM height) ++ "'>\n  <g>\n" ++
    "    <rect x='" ++ show 0 ++ "' y='" ++ show 0 ++ "' " ++
    "width='" ++ show (scaleM width) ++ "' height='" ++ show (scaleM height) ++
    "' style='stroke-width:3; stroke:black;fill:ghostwhite'/>\n"

ender :: String
ender = "  </g>\n</svg>"

--ring ::

rectangle :: Int -> Int -> Int -> Int -> Int -> Bool -> String
rectangle x y width height numero b =
    "    <rect x='" ++ show (scaleM x) ++ "' y='" ++ show (scaleM y) ++ "' " ++
    "width='" ++ show (scale width) ++ "' height='" ++ show (scale height) ++
    "' style='stroke-width:3; stroke:black;fill:" ++ (if b then "khaki" else "ghostwhite") ++
    "'/>\n" ++
    num numero (fromIntegral x + fromIntegral width/2) (fromIntegral y + fromIntegral height/2)

num :: Int -> Float -> Float -> String
num i x y = "    <text x='" ++ show (fscaleM x) ++ "' y='" ++ show (fscaleM y) ++
            "' fill='black'>" ++ show i ++ "</text>\n"


svgChar :: Int -> Int -> Char -> String
svgChar x y c = "    <text x='" ++ show (scaleM x) ++ "' y='" ++ show (scaleM y) ++
             "' fill='black'>" ++ [c] ++ "</text>\n"

line :: ((Int, Int), (Int, Int)) -> String
line ((x1, y1), (x2, y2))
    | y1 == y2 && x1 < x2 =
        "    <line x1='" ++ show (20 + scaleM x1) ++ "' y1='" ++ show (20 + scaleM y1) ++ 
        "' x2='" ++ show (20 + scaleM x2) ++ "' y2='" ++ show (20 + scaleM y2) ++ 
        "' style='stroke:black;stroke-width:2' />\n"
    | y1 == y2 =
        "    <line x1='" ++ show (20 + scaleM x1) ++ "' y1='" ++ show (20 + scaleM y1) ++ 
        "' x2='" ++ show (20 + scaleM x2) ++ "' y2='" ++ show (20 + scaleM y2) ++ 
        "' style='stroke:black;stroke-width:2' />\n"
    | y1 < y2 =
        "    <line x1='" ++ show (20 + scaleM x1) ++ "' y1='" ++ show (20 + scaleM y1) ++ 
        "' x2='" ++ show (20 + scaleM x2) ++ "' y2='" ++ show (20 + scaleM y2) ++ 
        "' style='stroke:black;stroke-width:2' />\n"
    | otherwise =
        "    <line x1='" ++ show (20 + scaleM x1) ++ "' y1='" ++ show (20 + scaleM y1) ++ 
        "' x2='" ++ show (20 + scaleM x2) ++ "' y2='" ++ show (20 + scaleM y2) ++ 
        "' style='stroke:black;stroke-width:2' />\n"