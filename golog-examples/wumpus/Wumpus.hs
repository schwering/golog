{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Main where

import Golog.Interpreter
import Golog.Macro
import Golog.Util

data Direction = North | East | South | West
data Sense = Stench | Breeze | Glitter | Scream
data A = Move | Turn | Pick | Shoot | Sense Sense Bool

locs :: [(Int, Int)]
locs = [(x,y) | x <- [1..4], y <- [1..4]]

validLoc :: (Int, Int) -> Bool
validLoc (x,y) = 1 <= x && x <= 4 && 1 <= y && y <= 4

succLoc :: Direction -> (Int, Int) -> (Int, Int)
succLoc North (x,y) = (x,y+1)
succLoc South (x,y) = (x,y-1)
succLoc East  (x,y) = (x+1,y)
succLoc West  (x,y) = (x-1,y)

env :: (Int, Int) -> [(Int, Int)]
env (x,y) = [(x,y+1), (x,y-1), (x+1,y), (x-1,y)]

instance BAT A where
   data Sit A = State { loc        :: (Int, Int)
                      , dir        :: Direction
                      , alive      :: [Bool]
                      , locW       :: [(Int, Int)]
                      , aliveW     :: Bool
                      , pits       :: [(Int, Int)]
                      , haveGold   :: Bool
                      , visited    :: [(Int, Int)]
                      , haveArrow  :: Bool
                      , goldPieces :: Int
                      }

   s0 = State { loc = (1,1)
              , dir = North
              , alive = [True]
              , locW = locs
              , aliveW = True
              , pits = locs
              , visited = [(1,1)]
              , haveArrow = True
              , goldPieces = 0
              }

   do_ Move  s = s{loc = succLoc (dir s) (loc s)
                  ,alive = locW s == succLoc (dir s) (loc s)
                  ,visited = succLoc (dir s) (loc s) : visited s}
   do_ Turn  s = s{dir = case dir s of North -> East
                                       East  -> South
                                       South -> West
                                       West  -> North}
   do_ Pick  s = s{goldPieces = goldPieces s + 1}
   do_ Shoot s = s{aliveW = if all (inDir (loc s) (dir s)) (locW s) then False else [True, False]
                  ,haveArrow = False}
   do_ (Sense Stench b)  s = s{locW = filter (\l -> (l `elem` env (loc s)) == b) (locW s)}
   do_ (Sense Breeze b)  s = s{pits = filter (\l -> (l `elem` env (loc s)) == b) (pits s)}
   do_ (Sense Glitter b) s = s{haveGold = b}
   do_ (Sense Scream b)  s = s{aliveW = b}

   poss Move        s = alive s && validLoc (succLoc (dir s) (loc s))
   poss Turn        s = alive s
   poss Pick        s = alive s && haveGold s
   poss Shoot       s = alive s && haveArrow s
   poss (Sense _ _) s = alive s

main :: IO ()
main = putStrLn "huhu"

