{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


-- | Basic action theory based on relative temporal measures using progression.

module RSTC.BAT.Progression (Prim(..), NTGCat(..), TTCCat(..), State(..),
                             Sit(..),
                             lookahead, ntgDiff, ttcDiff, quality, match,
                             ntgCats, ttcCats, nan, bestAccel,
                             sit2list, list2sit, inject, remove) where

import RSTC.Car
import RSTC.BAT.Base
import Interpreter.Golog
import qualified RSTC.Obs as O
import RSTC.Theorems
import Util.Interpolation
import Util.MemoCache

import Data.List
import Data.Array.IArray
import Data.Array.Unboxed

instance BAT (Prim Double) where
   data Sit (Prim Double) = State { histlen :: Int
                                  , history :: [Prim Double]
                                  , time'   :: Time Double
                                  , lane'   :: Array Car Lane
                                  , ntg'    :: UArray (Car, Car) (NTG Double)
                                  , ttc'    :: UArray (Car, Car) (TTC Double)
                                  }

   s0  = State { histlen = 0
               , history = []
               , time' = 0
               , lane' = array (minBound, maxBound) [(i, RightLane) | i <- [minBound..maxBound]] :: Array Car Lane
               , ntg' = array ((minBound, minBound), (maxBound,maxBound)) [((i,j), nan) | i <- [minBound..maxBound], j <- [minBound..maxBound]] :: UArray (Car,Car) (NTG Double)
               , ttc' = array ((minBound, minBound), (maxBound,maxBound)) [((i,j), nan) | i <- [minBound..maxBound], j <- [minBound..maxBound]] :: UArray (Car,Car) (TTC Double)
               }

   do_ a @ (Wait t)         s @ (State len as time'' lane'' ntg'' ttc'') =
      State (len+1) (a:as) (time'' + t)
            lane''
            (ntg'' // [((b, c), tntg (ntg s) (ttc s) t b c) | b <- cars, c <- cars])
            (ttc'' // [((b, c), tttc (ntg s) (ttc s) t b c) | b <- cars, c <- cars])

   do_ a @ (Accel b q)      s @ (State len as time'' lane'' ntg'' ttc'') =
      State (len+1) (a:as) time''
            lane''
            (ntg'' // ([((b, c), antg1 (ntg s) (ttc s) q b c) | c <- cars] ++ [((c, b), antg2 (ntg s) (ttc s) q c b) | c <- cars]))
            (ttc'' // ([((b, c), attc1 (ntg s) (ttc s) q b c) | c <- cars] ++ [((c, b), attc2 (ntg s) (ttc s) q c b) | c <- cars]))

   do_ a @ (LaneChange b l) (State len as time'' lane'' ntg'' ttc'') =
      State (len+1) (a:as) time''
            (lane'' // [(b, l)])
            ntg''
            ttc''

   do_ a @ (Init e)         (State len as time'' lane'' ntg'' ttc'') =
      State (len+1) (a:as) (O.time e)
            (lane'' // [(b, O.lane e b) | b <- cars])
            (ntg'' // [((b, c), O.ntg e b c) | b <- cars, c <- cars])
            (ttc'' // [((b, c), O.ttc e b c) | b <- cars, c <- cars])

   do_ a                    (State len as time'' lane'' ntg'' ttc'') =
      State (len+1) (a:as) time'' lane'' ntg'' ttc''

   poss (Wait t)             _ = t >= 0 && not (isNaN t)
   poss a @ (Accel _ q)      s = noDupe a s && not (isNaN q)
   poss a @ (LaneChange b l) s = noDupe a s && l /= lane s b
   poss (Init _)             _ = True
   poss (Prematch _)         _ = True
   poss (Match e)            s = match e s
   poss Abort                _ = False
   poss NoOp                 _ = True
   poss (Start _ _)          _ = True
   poss (End _ _)            _ = True
   poss (Msg _)              _ = True

   reward (Wait _)         _ = 0
   reward (Accel _ _)      _ = -0.01
   reward (LaneChange _ _) _ = -0.01
   reward (Init _)         _ = 0
   reward (Prematch _)     _ = 0
   reward (Match e)        s = 1 - sum ntgDiffs / genericLength ntgDiffs
      where ntgDiffs = map realToFrac [abs (ntgDiff s e b c) | b <- cars, c <- cars, b /= c]
            ttcDiffs = map realToFrac [abs (signum (ttc s b c) - signum (O.ttc e b c)) | b <- cars, c <- cars, b < c]
   reward Abort            _ = 0
   reward NoOp             _ = 0
   reward (Start _ _)      s = max 0 (1000 - 2 * (fromIntegral (sitlen s)))
   reward (End _ _)        s = case history s of (Match _ : _) -> 2 * (fromIntegral (histlen s - 1))
                                                 _             -> 0
   reward (Msg _)          _ = 0


instance State Double (Sit (Prim Double)) where
   time s = time' s
   lane s b = (lane' s) ! b
   ntg  s b c = (ntg' s) ! (b, c)
   ttc  s b c = (ttc' s) ! (b, c)


-- | Number of actions in a situation term.
sitlen :: Sit (Prim Double) -> Int
sitlen = histlen


-- | List of actions in situation term, ordered by their occurrence.
sit2list :: Sit (Prim Double) -> [Prim Double]
sit2list = reverse . history


-- | Situation term from the actions in list.
list2sit :: [Prim Double] -> Sit (Prim Double)
list2sit = append2sit s0


-- | Appends list of actions in given order to situation term as new actions.
append2sit :: Sit (Prim Double) -> [Prim Double] -> Sit (Prim Double)
append2sit s []     = s
append2sit s (a:as) = append2sit (do_ a s) as


-- | Injects a new action 'n' actions ago in the situation term.
inject :: Int -> (Prim Double) -> Sit (Prim Double) -> Sit (Prim Double)
inject n a s = list2sit (reverse (take n l ++ [a] ++ drop n l))
   where l = reverse (sit2list s)


-- | Removes the action 'n' actions ago in the situation term.
remove :: Int -> Sit (Prim Double) -> Sit (Prim Double)
remove n s = list2sit (reverse (take n l ++ drop (n+1) l))
   where l = reverse (sit2list s)


bestAccel :: Sit (Prim Double) -> Car -> Car -> Accel Double
bestAccel s b c = 0.5 * fx + 0.5 * gx
   where fx = let (f1, f2) = (f 2, f 3) in pick f1 f2 (nullAt id (canonicalize Recip  f1 0))
         gx = let (g1, g2) = (g 2, g 3) in pick g1 g2 (nullAt id (canonicalize Linear g1 0))
         pick f1 f2 xs = case sortBy (\x y -> compare (abs (f1 x + f2 x)) (abs (f1 y + f2 y))) xs of (x:_) -> x ; [] -> nan
         f n q = let s' = ss n q
                 in case history s' of (Match e : _) -> quality s e b c
                                       _             -> nan
         g n q = let s' = ss n q
                 in case history s' of (Match e : _) -> quality s e b c
                                       _             -> nan
         ss n q = append2sit s (Accel b q : (obsActions (history s) !! n))
         obsActions (Match e : _ )  = inits (obsActions' (O.wrap e))
         obsActions (Init e  : _ )  = inits (obsActions' (O.wrap e))
         obsActions (_       : as) = obsActions as
         obsActions' :: O.Wrapper a -> [Prim a]
         obsActions' (O.Wrapper e) =
            case O.next e of Just e' -> Wait (O.time e' - O.time e) :
                                        Match e' :
                                        obsActions' (O.wrap e')
                             Nothing -> []


noDupe :: Prim Double -> Sit (Prim Double) -> Bool
noDupe a s = noDupe' a (history s)
   where noDupe' a' @ (Accel b _) (a:as) = case a of Wait _    -> True
                                                     Accel c _ -> c < b
                                                     _         -> noDupe' a' as
         noDupe' a' @ (LaneChange b _) (a:as) = case a of Wait _         -> True
                                                          LaneChange c _ -> c < b
                                                          _              -> noDupe' a' as
         noDupe' _ [] = True
         noDupe' _ _  = error "RSTC.BAT.Progression.noDupe: neither Accel nor LaneChange"

