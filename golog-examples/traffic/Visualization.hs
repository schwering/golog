module Visualization where

import qualified Car as C
import qualified Data.Map as Map
import qualified World as R

roadSymbol :: R.Direction -> Char
roadSymbol R.North     = '\8593'
roadSymbol R.East      = '\8594'
roadSymbol R.West      = '\8592'
roadSymbol R.South     = '\8595'
roadSymbol R.NorthEast = '\8599'
roadSymbol R.NorthWest = '\8598'
roadSymbol R.SouthEast = '\8600'
roadSymbol R.SouthWest = '\8601'

draw :: R.World Int -> IO ()
draw w = draw' 0 0
   where m  = R.streetLut w
         cs = R.carLut w
         display x y = case (Map.lookup (x,y) cs, Map.lookup (x,y) m) of
                            (Just c,  Just (_,ls,s)) -> putStr (color ls s) >> putChar c >> putStr knrm
                            (Nothing, Just (d,ls,s)) -> putStr (color ls s) >> putChar (roadSymbol d) >> putStr knrm
                            (Nothing, Nothing)       -> putChar ' '
                            (Just _,  Nothing)       -> error "draw: car outside road"
         color (Just R.Green)  _ = kgrn
         color (Just R.Yellow) _ = kyel
         color Nothing          s = R.color s
         draw' x y | y > R.maxY w  = return ()
                   | x < R.maxX w  = display x y  >> draw' (x+1) y
                   | x == R.maxX w = putChar '\n' >> draw' 0 (y+1)
                   | otherwise     = error "draw"

knrm, kred, kgrn, kyel, kblu, kmag, kcyn, kwht :: R.ColorString
knrm = "\x1B[0m"
kred = "\x1B[31m"
kgrn = "\x1B[32m"
kyel = "\x1B[33m"
kblu = "\x1B[34m"
kmag = "\x1B[35m"
kcyn = "\x1B[36m"
kwht = "\x1B[37m"

