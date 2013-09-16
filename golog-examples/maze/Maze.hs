{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls, UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Prelude hiding (Left, Right)
import Control.Concurrent (threadDelay);
import Data.List (sortBy)
import Golog.Interpreter
import Golog.Macro
import Golog.Util
import qualified Random


{- Maze functions: -}

data Point = P Int Int deriving (Show, Eq)

roomSize, roomHeight, roomWidth :: Int
roomSize   = 10
roomHeight = roomSize
roomWidth  = roomHeight

startPos :: Point
startPos = P 0 0

goalPos :: Point
goalPos = P (2 * roomWidth - 1) (2 * roomHeight - 1)

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
up'     (P x y) = P x (y - 1)
down'   (P x y) = P x (y + 1)
left'   (P x y) = P (x - 1) y
right'  (P x y) = P (x + 1) y

isValidNeighborOf :: Point -> Point -> Bool
isValidNeighborOf (P x' y') p@(P x y)
   | x' == x   && y' == y-1 = not (atUpperWall y) || atVerticalDoor p
   | x' == x   && y' == y+1 = not (atLowerWall y) || atVerticalDoor p
   | x' == x-1 && y' == y   = not (atLeftWall  x) || atHorizontalDoor p
   | x' == x+1 && y' == y   = not (atRightWall x) || atHorizontalDoor p
   | otherwise              = False

dist :: Point -> Point -> Double
dist = --distManhattan
       distEuclidean

distEuclidean :: Point -> Point -> Double
distEuclidean (P x1 y1) (P x2 y2) = sqrt (fromIntegral $ x*x + y*y)
   where x = x1 - x2
         y = y1 - y2

distManhattan :: Point -> Point -> Double
distManhattan (P x1 y1) (P x2 y2) = fromIntegral $ abs (x1 - x2) + abs (y1 - y2)


data Prim a = Up | Down | Left | Right | Test (Cond a) deriving (Show, Eq)

newtype Cond a = Cond (Sit (Prim a) -> Bool)

instance Show (Cond a) where
   show (Cond _) = "Cond <...>"

instance Eq (Cond a) where
   _ == _ = error "compare: Conditions are not comparable"

instance TestAction (Prim a) where
   testAction = Test . Cond

class HistBAT (Prim a) => MazeBAT a where
   pos          :: Sit (Prim a) -> Point
   unvisited    :: Point -> Sit (Prim a) -> Bool
   visited      :: Sit (Prim a) -> [Point]
   randomSupply :: MazeBAT a => Sit (Prim a) -> Random.Supply
   reward'      :: Sit (Prim a) -> Reward (Prim a)

   unvisited p s = p `notElem` visited s


{- Regressive BAT: -}

data Regr

instance BAT (Prim Regr) where
   data Sit (Prim Regr) = S0 | Do (Prim Regr) (Sit (Prim Regr)) deriving Show
   s0 = S0
   do_ (Test _) s = s
   do_ a        s = Do a s
   poss = allowedAction

instance HistBAT (Prim Regr) where
   history S0       = []
   history (Do a s) = a : history s

instance MazeBAT Regr where
   pos = pos' . history
      where pos' []     = startPos
            pos' (a:as) = newPos a (pos' as)

   visited s = scanr (\f p -> f p) startPos (map newPos (history s))

   reward' S0       = 0
   reward' (Do a s) = actionReward a s + reward' s

   randomSupply s = foldr (\f rs -> f rs) startRandomSupply (map newRandomSupply (history s))


{- Progressive BAT: -}

data Progr

instance BAT (Prim Progr) where
   data Sit (Prim Progr)       = Sit [Point] (Reward (Prim Progr)) [Prim Progr] Random.Supply deriving Show
   s0                          = Sit [startPos] 0 [] startRandomSupply

   do_  (Test _) s                       = s
   do_  a        s@(Sit ps@(p:_) r m rs) = Sit (newPos a p : ps)
                                               (r + actionReward a s)
                                               (a : m)
                                               (newRandomSupply a rs)
   do_  _        (Sit []         _ _ _)  = error "do_: empty point history"

   poss = allowedAction

instance HistBAT (Prim Progr) where
   history (Sit _ _ m _) = m

instance MazeBAT Progr where
   pos (Sit (p:_) _ _ _) = p
   pos (Sit _     _ _ _) = error "pos: empty point history"

   visited (Sit ps _ _ _) = ps

   reward' (Sit _ r _ _) = r

   randomSupply (Sit _ _ _ rs) = rs


{- Common BAT functions: -}

allowedAction :: MazeBAT a => Prim a -> Sit (Prim a) -> Bool
allowedAction (Test (Cond f)) s = f s
allowedAction a               s = p' `isValidNeighborOf` p && unvisited p' s
   where p' = newPos a p
         p  = pos s

actionReward :: MazeBAT a => Prim a -> Sit (Prim a) -> Reward (Prim a)
actionReward (Test _) s = 0
actionReward a s = Reward $ (improv - penalty a (history s) / 9) / normf - 100
      where oldRem = dist goalPos (pos s)
            newRem = dist goalPos (newPos a (pos s)) 
            improv = oldRem - newRem
            normf  = fromIntegral $ sitlen s + 1
            penalty Up       (Up:_)    = 0
            penalty Up       (Down:_)  = 2
            penalty Up       (_:_)     = 1
            penalty Down     (Down:_)  = 0
            penalty Down     (Up:_)    = 2
            penalty Down     (_:_)     = 1
            penalty Left     (Left:_)  = 0
            penalty Left     (Right:_) = 2
            penalty Left     (_:_)     = 1
            penalty Right    (Right:_) = 0
            penalty Right    (Left:_)  = 2
            penalty Right    (_:_)     = 1
            penalty (Test _) _         = error "penalty: Test action"
            penalty _        []        = 0
   --dist (pos s) goalPos - dist (newPos a (pos s)) goalPos
   --abs (dist startPos goalPos - dist (newPos a (pos s)) goalPos)**1 -
   --abs (dist startPos goalPos - dist (pos s) goalPos)**1

instance (BAT (Prim a), MazeBAT a) => DTBAT (Prim a) where
   newtype Reward (Prim a) = Reward Double deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat, Show)
   reward = reward'

instance (BAT (Prim a), MazeBAT a) => IOBAT (Prim a) IO where
   syncA a s = do let s' = do_ a s
                  --if a == Down then threadDelay (1*1000*1000) else return ()
                  putStrLn $ (if pos s' == goalPos then " *** " else " ... ") ++
                             show a ++ ": " ++
                             show (pos s', dist goalPos (pos s'), reward s')
                  return s'

newPos :: (Prim a) -> Point -> Point
newPos Up       p = up' p
newPos Down     p = down' p
newPos Left     p = left' p
newPos Right    p = right' p
newPos (Test _) _ = error "newPos: Test action"

startRandomSupply :: Random.Supply
startRandomSupply = Random.init 3

newRandomSupply :: Prim a -> Random.Supply -> Random.Supply
newRandomSupply Up       rs = Random.shuffle  7 $ snd $ Random.random $ rs
newRandomSupply Down     rs = Random.shuffle 13 $ snd $ Random.random $ rs
newRandomSupply Left     rs = Random.shuffle 19 $ snd $ Random.random $ rs
newRandomSupply Right    rs = Random.shuffle 31 $ snd $ Random.random $ rs
newRandomSupply (Test _) rs = error "newRandomSupply: Test action"

random :: (BAT (Prim a), MazeBAT a) => Sit (Prim a) -> Int
random s = fst $ Random.random (randomSupply s)

up :: (BAT (Prim a), MazeBAT a) => Sit (Prim a) -> Prim a
up s = best (Down,Left,Right,Up) s

down :: (BAT (Prim a), MazeBAT a) => Sit (Prim a) -> Prim a
down s = best (Up,Left,Right,Down) s

left :: (BAT (Prim a), MazeBAT a) => Sit (Prim a) -> Prim a
left s = best (Up,Down,Right,Left) s

right :: (BAT (Prim a), MazeBAT a) => Sit (Prim a) -> Prim a
right s = best (Up,Down,Left,Right) s

best :: (BAT (Prim a), MazeBAT a) => (Prim a,Prim a,Prim a,Prim a) -> Sit (Prim a) -> (Prim a)
best (a0,a1,a2,a3) s |  0 <= pct && pct < 10 && poss a0 s = a0
                     | 10 <= pct && pct < 30 && poss a1 s = a1
                     | 30 <= pct && pct < 50 && poss a2 s = a2
                     | otherwise                          = a3
   where r   = random s
         pct = r `mod` 100

lookahead :: Depth
lookahead = 5

main :: IO ()
main = do
   let prog :: Prog (Prim Regr)
       prog  = star (choice [ primf up
                            , primf down
                            , primf left
                            , primf right]) `Seq`
               test (\s -> pos s == goalPos)
       tree = treeDTIO lookahead prog s0 :: ConfIO (Prim Regr) IO
       mode c = case history (sit c) of []         -> Offline BFS
                                        Test _ : _ -> Offline BFS
                                        Left   : _ -> Online
                                        Right  : _ -> Offline DFS
                                        Up     : _ -> Offline BFS
                                        Down   : _ -> Offline BFS
                                        _          -> Online
       mode _ = Online
       --confs = transTrace' tree
   putStrLn $ show $ startPos
   putStrLn $ show $ goalPos
{-
   mapM_ (\s -> putStrLn $ (if pos s == goalPos then " *** " else " ... ") ++
            show (pos s, dist goalPos (pos s), reward s
                  --,case s of Do a s' -> (pos s `elem` visited s', visited s')
                  )) $ map sit confs
-}
   --let conf = last confs
   Just conf <- dooIO mode tree
   -- The following lines just test that subsequent syncs have no effect:
   let s = sit conf
   putStrLn $ "Actions: " ++ show (sitlen s)
   draw s

draw :: MazeBAT a => Sit (Prim a) -> IO ()
draw s = draw' 0 0 visPs
   where --allPs = [(x,y) | x <- [0..2*roomWidth], y <- [0..2*roomHeight]]
         visPs = sortBy cmp (visited s)
         cmp (P x1 y1) (P x2 y2) = compare (y1,x1) (y2,x2)
         draw' :: Int -> Int -> [Point] -> IO ()
         draw' _ _ []            = putChar '\n'
         draw' x y ps@((P x' y') : ps')
            | y == y' && x == x' = putChar 'X'  >> draw' (x+1) y ps'
            | y == y' && x <  x' = putChar ' '  >> draw' (x+1) y ps
            | y <  y'            = putChar '\n' >> draw' 0 (y+1) ps
            | otherwise          = error $ "draw' "++ show (x,y) ++" "++ show (x',y')

