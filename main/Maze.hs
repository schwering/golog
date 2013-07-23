{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}

module Main (main) where

import Prelude hiding (Left, Right)
import Interpreter.Golog2
import Interpreter.Golog2Util
import qualified Util.Random as Random

data Point = P Int Int deriving (Show, Eq)

roomSize, roomHeight, roomWidth :: Int
roomSize   = 20
roomHeight = roomSize
roomWidth  = roomHeight

start :: Point
start = P 0 0

goal :: Point
goal = P (2 * roomWidth - 1) (2 * roomHeight - 1)

inMaze :: Point -> Bool
inMaze (P x y) = 0 <= x && x < 2 * roomWidth &&
                 0 <= y && y < 2 * roomHeight

atUpperWall, atLowerWall, atLeftWall, atRightWall :: Int -> Bool
atUpperWall y = y     `mod` roomHeight == 0
atLowerWall y = (y+1) `mod` roomHeight == 0
atLeftWall  x = x     `mod` roomWidth  == 0
atRightWall x = (x+1) `mod` roomWidth  == 0

atVerticalDoor, atHorizontalDoor :: Point -> Bool
atVerticalDoor   (P x y) = x `mod` roomWidth  == roomWidth  `div` 2 && abs (y - roomHeight) <= 1
atHorizontalDoor (P x y) = y `mod` roomHeight == roomHeight `div` 2 && abs (x - roomWidth)  <= 1

up', down', left', right' :: Point -> Point
up' (P x y)     = P x (y - 1)
down' (P x y)   = P x (y + 1)
left'  (P x y)  = P (x - 1) y
right'  (P x y) = P (x + 1) y

validNeighbors :: Point -> [Point]
validNeighbors p@(P x y) =
   ( if not (atUpperWall y) || atVerticalDoor   p then [up'    p] else [] ) ++
   ( if not (atLowerWall y) || atVerticalDoor   p then [down'  p] else [] ) ++
   ( if not (atLeftWall  x) || atHorizontalDoor p then [left'  p] else [] ) ++
   ( if not (atRightWall x) || atHorizontalDoor p then [right' p] else [] )

isValidNeighborOf :: Point -> Point -> Bool
isValidNeighborOf (P x' y') p@(P x y)
   | x' == x   && y' == y-1 = not (atUpperWall y) || atVerticalDoor p
   | x' == x   && y' == y+1 = not (atLowerWall y) || atVerticalDoor p
   | x' == x-1 && y' == y   = not (atLeftWall  x) || atHorizontalDoor p
   | x' == x+1 && y' == y   = not (atRightWall x) || atHorizontalDoor p
   | otherwise              = False

dist :: Point -> Point -> Double
dist (P x1 y1) (P x2 y2) = sqrt (dx*dx + dy*dy)
   where dx = fromIntegral $ x1 - x2
         dy = fromIntegral $ y1 - y2

dist2 :: Point -> Point -> Double
dist2 (P x1 y1) (P x2 y2) = abs (x1 - x2) + abs (y1 - y2)

data Prim a = Up | Down | Left | Right deriving Show

class BAT a => MemBAT a where
   memory :: Sit a -> [a]
   rewardSum :: (DTBAT a, MemBAT a) => Sit a -> Reward

instance BAT (Prim ()) where
   data Sit (Prim ()) = S0 | Do (Prim ()) (Sit (Prim ())) deriving Show
   s0  = S0
   do_ = Do
   poss a s = let p  = pos s
                  p' = newPos a p
              in p' `isValidNeighborOf` p && unvisited p' s

instance DTBAT (Prim ()) where
   reward a s = (dist start goal - dist (newPos a (pos s)) goal)**4 -
                (dist start goal - dist (pos s) goal)**4 - 1

instance MemBAT (Prim ()) where
   memory S0       = []
   memory (Do a s) = a : memory s

   rewardSum S0       = 0
   rewardSum (Do a s) = rewardSum s + reward a s


newPos :: (Prim a) -> Point -> Point
newPos Up    p = up' p
newPos Down  p = down' p
newPos Left  p = left' p
newPos Right p = right' p

sitlen :: MemBAT a => Sit a -> Int
sitlen = length . memory

randomSupply :: MemBAT (Prim a) => Sit (Prim a) -> Random.Supply
randomSupply s = rs (memory s)
   where rs []           = Random.init 3
         rs (Up    : as) = Random.shuffle  7 $ snd $ Random.random $ rs as
         rs (Down  : as) = Random.shuffle 13 $ snd $ Random.random $ rs as
         rs (Left  : as) = Random.shuffle 19 $ snd $ Random.random $ rs as
         rs (Right : as) = Random.shuffle 31 $ snd $ Random.random $ rs as

random :: MemBAT (Prim a) => Sit (Prim a) -> Int
random s = fst $ Random.random (randomSupply s)

up :: MemBAT (Prim a) => Sit (Prim a) -> Prim a
up s = opt (Down,Left,Right,Up) s

down :: MemBAT (Prim a) => Sit (Prim a) -> Prim a
down s = opt (Up,Left,Right,Down) s

left :: MemBAT (Prim a) => Sit (Prim a) -> Prim a
left s = opt (Up,Down,Right,Left) s

right :: MemBAT (Prim a) => Sit (Prim a) -> Prim a
right s = opt (Up,Down,Left,Right) s

opt :: MemBAT (Prim a) => (Prim a,Prim a,Prim a,Prim a) -> Sit (Prim a) -> (Prim a)
opt (a0,a1,a2,a3) s |  0 <= pct && pct < 10 && poss a0 s = a0
                    | 10 <= pct && pct < 30 && poss a1 s = a1
                    | 30 <= pct && pct < 50 && poss a2 s = a2
                    | otherwise                          = a3
   where r   = random s
         pct = r `mod` 100

lookahead :: Depth
lookahead = 7

pos :: MemBAT (Prim a) => Sit (Prim a) -> Point
pos = pos' . memory
   where pos' []     = start
         pos' (a:as) = newPos a (pos' as)

unvisited :: MemBAT (Prim a) => Point -> Sit (Prim a) -> Bool
unvisited p s = p `notElem` visited s

visited :: MemBAT (Prim a) => Sit (Prim a) -> [Point]
visited s = scanr (\f p -> f p) start (map newPos (memory s))

main :: IO ()
main = do
   let prog  = star $ Nondet [primf up, primf down, primf left, primf right]
   --let prog  = star $ Nondet [prim Up, prim Down, prim Left, prim Right]
            :: Prog (Prim ())
       tree  = treeDT lookahead prog s0
       confs = do2 tree
   putStrLn $ show $ start
   putStrLn $ show $ goal
   mapM_ (\s -> putStrLn $ (if pos s == goal then "Goal: " else "  ... ") ++
            show (pos s, dist goal (pos s), rewardSum s
                  --,case s of Do a s' -> (pos s `elem` visited s', visited s')
                  )) $ map sit confs

