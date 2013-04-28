{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


-- | Basic action theory based on relative temporal measures using regression.

module RSTC.BAT.Regression (Prim(..), NTGCat(..), TTCCat(..), State(..),
                            Sit(..),
                            lookahead, ntgDiff, ttcDiff, quality, match,
                            bestAccel, ntgCats, ttcCats, nan,
                            history, sit2list, list2sit, predsit,
                            inject, remove) where

import RSTC.Car
import RSTC.BAT.Base
import Interpreter.Golog
import qualified RSTC.Obs as O
import RSTC.Theorems
import Util.MemoCache

instance RealFloat a => BAT (Prim a) where
   data Sit (Prim a) = S0 | Do (Prim a) (Sit (Prim a))

   s0  = S0
   do_ = Do
   poss = defaultPoss
   reward = defaultReward


instance RealFloat a => State a where
   time = memo1 1 time'
      where time' :: RealFloat a => Sit (Prim a) -> Time a
            time' (Do (Wait t) s) = t + (time s)
            time' (Do (Init e) _) = O.time e
            time' (Do _ s)        = time s
            time' S0              = 0

   lane = memo2 1 lane'
      where lane' :: RealFloat a => Sit (Prim a) -> Car -> Lane
            lane' (Do (LaneChange c l) _) b | b == c = l
            lane' (Do (Init e) _)         b          = O.lane e b
            lane' (Do _ s)                b          = lane s b
            lane' S0                      _          = RightLane

   ntg = memo3 1 ntg'
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

   ttc = memo3 2 ttc'
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


instance RealFloat a => HistState a where
   history S0       = []
   history (Do a s) = a : history s

   histlen S0       = 0
   histlen (Do _ s) = 1 + (histlen s)

   predsit S0       = error "RSTC.BAT.Regression.subsit: S0"
   predsit (Do _ s) = s

