module Main where

import Data.List (minimumBy)
import qualified Data.Map as Map
import Golog.Interpreter
import Golog.Macro
import Golog.Util
import qualified Random as R

data Direction = North | East | South | West | NorthEast | NorthWest | SouthEast | SouthWest

data Street a = Street { streetName      :: String
                       , streetColor     :: String
                       , streetStart     :: (a, a)
                       , streetEnd       :: (a, a)
                       , streetPoints    :: [((a, a), Direction)]
                       }

type World a = [Street a]

randoms :: [Int]
randoms = randoms' (R.init 3)
   where randoms' s = let (r,s') = R.random s in r : randoms' s'

neighbors :: (Eq a, Num a) => (a, a) -> [(a, a)]
neighbors (x,y) = [(x',y') | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1], (x,y) /= (x',y')]

genPoints :: (Ord a, Num a) => Street a -> Street a
genPoints s = s{streetPoints = points (streetStart s) (streetEnd s)}
   where points p0 p2 | p0 == p2  = []
                      | otherwise = (p1, d) : points p1 p2
            where p1 = minimumBy (\q1 q2 -> compare (manhattan q1 p2) (manhattan q2 p2)) (neighbors p0)
                  d  | fst p0 == fst p1 && snd p0 <  snd p1 = East
                     | fst p0 == fst p1 && snd p0 >  snd p1 = West
                     | fst p0 <  fst p1 && snd p0 == snd p1 = South
                     | fst p0 >  fst p1 && snd p0 == snd p1 = North
                     | fst p0 <  fst p1 && snd p0 <  snd p1 = SouthEast
                     | fst p0 <  fst p1 && snd p0 >  snd p1 = SouthWest
                     | fst p0 >  fst p1 && snd p0 <  snd p1 = NorthEast
                     | fst p0 >  fst p1 && snd p0 >  snd p1 = NorthWest

manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan (x0,y0) (x1,y1) = abs (x1 - x0) + abs (y1 - y0)

bounds :: Ord a => World a -> ((a, a), (a, a))
bounds w = (minimum $ map (minimum . map fst . streetPoints) w,
            maximum $ map (maximum . map fst . streetPoints) w)

world :: World Int
world = [ genPoints (Street "1" kred (0,5) (0,60) undefined)
        , genPoints (Street "1" kblu (10,70) (10,3) undefined)
        , genPoints (Street "1" kmag (7,0) (20,20) undefined)
        , genPoints (Street "1" kyel (12,60) (25,0) undefined)
        ]

world2lut :: Ord a => World a -> Map.Map (a, a) (Direction, Street a)
world2lut = foldr addStreet Map.empty
   where addStreet s m0 = foldr (\(p,d) m -> Map.insert p (d,s) m) m0 (streetPoints s)

symbol :: Direction -> Char
symbol North     = '\8593'
symbol East      = '\8594'
symbol West      = '\8592'
symbol South     = '\8595'
symbol NorthEast = '\8599'
symbol NorthWest = '\8598'
symbol SouthEast = '\8600'
symbol SouthWest = '\8601'

draw :: World Int -> IO ()
draw w = draw' 0 0
   where m = world2lut w
         print x y = case Map.lookup (x,y) m of
                          Just (d,s) -> putStr (streetColor s) >> putChar (symbol d) >> putStr knrm
                          Nothing    -> putChar ' '
         (xMax, yMax) = (60, 80)
         draw' x y | x > xMax  = return ()
                   | y < yMax  = print x y    >> draw' x (y+1)
                   | y == yMax = putChar '\n' >> draw' (x+1) 0
                       
{-
   where draw' :: Int -> Int -> [((Int, Int), String)] -> IO ()
         draw' _ _ []            = putChar '\n'
         draw' x y ps@(((x',y'),c) : ps')
            | y == y' && x == x' = putStr c    >> putChar 'X'  >> draw' (x+1) y ps'
            | y == y' && x <  x' = putStr knrm >> putChar ' '  >> draw' (x+1) y ps
            | y <  y'            =                putChar '\n' >> draw' 0 (y+1) ps
            | otherwise          = error $ "draw' "++ show (x,y) ++" "++ show (x',y')
-}

main :: IO ()
--main = putStrLn $ show $ map streetPoints world
--main = putStrLn $ show $ Map.keys $ world2lut world
main = draw world

knrm, kred, kgrn, kyel, kblu, kmag, kcyn, kwht :: String
knrm = "\x1B[0m"
kred = "\x1B[31m"
kgrn = "\x1B[32m"
kyel = "\x1B[33m"
kblu = "\x1B[34m"
kmag = "\x1B[35m"
kcyn = "\x1B[36m"
kwht = "\x1B[37m"

