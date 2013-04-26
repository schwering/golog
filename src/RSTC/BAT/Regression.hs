{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


-- | Basic action theory based on relative temporal measures using regression.

module RSTC.BAT.Regression (Prim(..), NTGCat(..), TTCCat(..), State(..),
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

instance RealFloat a => BAT (Prim a) where
   data Sit (Prim a) = S0 | Do (Prim a) (Sit (Prim a))

   s0  = S0
   do_ = Do

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

   reward (Wait _)         _                = 0
   reward (Accel _ _)      _                = -0.01
   reward (LaneChange _ _) _                = -0.01
   reward (Init _)         _                = 0
   reward (Prematch _)     _                = 0
   reward (Match e)        s                = 1 - sum ntgDiffs / genericLength ntgDiffs
      where ntgDiffs = map realToFrac [abs (ntgDiff s e b c) | b <- cars, c <- cars, b /= c]
            ttcDiffs = map realToFrac [abs (signum (ttc s b c) - signum (O.ttc e b c)) | b <- cars, c <- cars, b < c]
   reward Abort            _                = 0
   reward NoOp             _                = 0
   reward (Start _ _)      s                = max 0 (1000 - 2 * (fromIntegral (sitlen s)))
   reward (End _ _)        (Do (Match _) s) = 2 * (fromIntegral (sitlen s))
   reward (End _ _)        _                = 0
   reward (Msg _)          _                = 0


instance RealFloat a => State a (Sit (Prim a)) where
   time = time'
      where time' :: RealFloat a => Sit (Prim a) -> Time a
            time' (Do (Wait t) s) = t + (time s)
            time' (Do (Init e) s) = O.time e
            time' (Do _ s)        = time s
            time' S0              = 0

   lane = lane'
      where lane' :: RealFloat a => Sit (Prim a) -> Car -> Lane
            lane' (Do (LaneChange c l) _) b | b == c = l
            lane' (Do (Init e) _)         b          = O.lane e b
            lane' (Do _ s)                b          = lane s b
            lane' S0                      _          = RightLane

   ntg  = memo3 1 ntg'
      where ntg' :: RealFloat a => Sit (Prim a) -> Car -> Car -> NTG a
            ntg' _                  b c | b == c = nan
            ntg' (Do (Wait t) s)    b c          = orTrans
                                                      (tntg (ntg s) (ttc s) t)
                                                      (ntgTrans (tntg (ntg s) (ttc s) t)
                                                                (tttc (ntg s) (ttc s) t))
                                                      b c
            ntg' (Do (Accel d q) s) b c | b == d = antg1 (ntg s) (ttc s) q b c
                                        | c == d = antg2 (ntg s) (ttc s) q b c
            ntg' (Do (Init e) _)    b c          = O.ntg e b c
            ntg' (Do _        s)    b c          = ntg s b c
            ntg' S0                 _ _          = nan

   ttc  = memo3 2 ttc'
      where ttc' :: RealFloat a => Sit (Prim a) -> Car -> Car -> NTG a
            ttc' _                  b c | b == c = nan
            ttc' (Do (Wait t) s)    b c          = tttc (ntg s) (ttc s) t b c
            ttc' (Do (Accel d q) s) b c | b == d = orTrans
                                                      (attc1 (ntg s) (ttc s) q)
                                                      (ttcTrans (antg1 (ntg s) (ttc s) q)
                                                                (attc1 (ntg s) (ttc s) q))
                                                      b c
                                        | c == d = orTrans
                                                      (attc2 (ntg s) (ttc s) q)
                                                      (ttcTrans (antg2 (ntg s) (ttc s) q)
                                                                (attc2 (ntg s) (ttc s) q))
                                                      b c
            ttc' (Do (Init e) _)    b c          = O.ttc e b c
            ttc' (Do _        s)    b c          = ttc s b c
            ttc' S0                 _ _          = nan


-- | Number of actions in a situation term.
sitlen :: Sit (Prim a) -> Int
sitlen (Do _ s) = 1 + (sitlen s)
sitlen S0       = 0


-- | List of actions in situation term, ordered by their occurrence.
sit2list :: Sit (Prim a) -> [Prim a]
sit2list S0 = []
sit2list (Do a s) = (sit2list s) ++ [a]


-- | Situation term from the actions in list.
list2sit :: RealFloat a => [Prim a] -> Sit (Prim a)
list2sit = append2sit s0


-- | Appends list of actions in given order to situation term as new actions.
append2sit :: RealFloat a => Sit (Prim a) -> [Prim a] -> Sit (Prim a)
append2sit s []     = s
append2sit s (a:as) = append2sit (do_ a s) as


-- | Injects a new action 'n' actions ago in the situation term.
inject :: Int -> (Prim a) -> Sit (Prim a) -> Sit (Prim a)
inject 0 a s         = Do a s
inject n a (Do a' s) = Do a' (inject (n-1) a s)
inject _ a S0        = Do a S0


-- | Removes the action 'n' actions ago in the situation term.
remove :: Int -> Sit (Prim a) -> Sit (Prim a)
remove 0 (Do _ s) = s
remove n (Do a s) = Do a (remove (n-1) s)
remove _ S0       = S0


bestAccel :: RealFloat a => Sit (Prim a) -> Car -> Car -> Accel a
bestAccel s b c = 0.5 * fx + 0.5 * gx
--   where fx = pick (nullAt id (canonicalizeSum Recip  (f 2) (f 3) 0))
--         gx = pick (nullAt id (canonicalizeSum Linear (g 2) (g 3) 0))
--   where fx = let (f1, f2) = (f 2, f 3) in pick f1 f2 (nullAt id (canonicalizeSum Recip  f1 f2 0))
--         gx = let (g1, g2) = (g 2, g 3) in pick g1 g2 (nullAt id (canonicalizeSum Linear g1 g2 0))
   where fx = let (f1, f2) = (f 2, f 3) in pick f1 f2 (nullAt id (canonicalize Recip  f1 0))
         gx = let (g1, g2) = (g 2, g 3) in pick g1 g2 (nullAt id (canonicalize Linear g1 0))
         pick f1 f2 xs = case sortBy (\x y -> compare (abs (f1 x + f2 x)) (abs (f1 y + f2 y))) xs of (x:_) -> x ; [] -> nan
         f n q = case ss n q of
                      s @ (Do (Match e) _) -> quality s e b c
                      _                    -> nan
         g n q = case ss n q of
                      s @ (Do (Match e) _) -> quality s e c b
                      _                    -> nan
         ss n q = append2sit s (Accel b q : (obsActions s !! n))
         obsActions (Do (Match e) _)  = inits (obsActions' (O.wrap e))
         obsActions (Do (Init e)  _)  = inits (obsActions' (O.wrap e))
         obsActions (Do _         s') = obsActions s'
         obsActions' :: O.Wrapper a -> [Prim a]
         obsActions' (O.Wrapper e) =
            case O.next e of Just e' -> Wait (O.time e' - O.time e) :
                                        Match e' :
                                        obsActions' (O.wrap e')
                             Nothing -> []


noDupe :: Prim a -> Sit (Prim a) -> Bool
noDupe a' @ (Accel b _) (Do a s) = case a
   of Wait _         -> True
      Accel c _      -> c < b
      _              -> noDupe a' s
noDupe a' @ (LaneChange b _) (Do a s) = case a
   of Wait _         -> True
      LaneChange c _ -> c < b
      _              -> noDupe a' s
noDupe _ S0 = True
noDupe _ _ = error "RSTC.BAT.Regression.noDupe: neither Accel nor LaneChange"

