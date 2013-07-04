{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Basic action theory based on relative temporal measures using progression.

module RSTC.BAT.Progression (Qty, Wrapper(..),
                             Prim(..), NTGCat(..), TTCCat(..), State(..), HistState(..),
                             lookahead, ntgDiff, ttcDiff, quality, match,
                             bestAccel, ntgCats, ttcCats, nan,
                             inject, remove) where

import RSTC.Car
import RSTC.BAT.Base
import Interpreter.Golog
import qualified RSTC.Obs as O
import RSTC.Theorems

import Data.Array.IArray
import Data.Array.Unboxed

-- | A type duplicate.
-- We need this type to avoid orphan instances and have regression and
-- progression coexist.
newtype Qty a = Qty a
   deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)

instance Show a => Show (Qty a) where
   show (Qty x) = show x

instance Wrapper Qty where
   wrap = Qty
   unwrap (Qty x) = x


instance BAT (Prim (Qty Double)) where
   data Sit (Prim (Qty Double)) = State Int -- History length
                                  [Prim (Qty Double)] -- History
                                  (Time Double) -- Current time
                                  (Array Car Lane) -- Lanes
                                  (UArray (Car, Car) (NTG Double)) -- NTGs
                                  (UArray (Car, Car) (TTC Double)) -- TTCs

   s0 = State 0 [] 0
               (array (minBound, maxBound) [(b, RightLane) | b <- [minBound..maxBound]])
               (array (minBound, maxBound) [((b,c), nan) | b <- [minBound..maxBound], c <- [minBound..maxBound]])
               (array (minBound, maxBound) [((b,c), nan) | b <- [minBound..maxBound], c <- [minBound..maxBound]])

   do_ a @ (Wait t)         s @ (State len as time' lane' ntg' ttc') =
      State (len+1) (a:as) (time' + (unwrap t))
            lane'
            (ntg' // [((b, c), unwrap $ tntg (ntg s) (ttc s) t b c) | b <- cars, c <- cars])
            (ttc' // [((b, c), unwrap $ tttc (ntg s) (ttc s) t b c) | b <- cars, c <- cars])

   do_ a @ (Accel b q)      s @ (State len as time' lane' ntg' ttc') =
      State (len+1) (a:as) time'
            lane'
            (ntg' // ([((b, c), unwrap $ antg1 (ntg s) (ttc s) q b c) | c <- cars] ++
                      [((c, b), unwrap $ antg2 (ntg s) (ttc s) q c b) | c <- cars]))
            (ttc' // ([((b, c), unwrap $ attc1 (ntg s) (ttc s) q b c) | c <- cars] ++
                      [((c, b), unwrap $ attc2 (ntg s) (ttc s) q c b) | c <- cars]))

   do_ a @ (LaneChange b l) (State len as time' lane' ntg' ttc') =
      State (len+1) (a:as) time'
            (lane' // [(b, l)])
            ntg'
            ttc'

   do_ a @ (Init e)         (State len as _ lane' ntg' ttc') =
      State (len+1) (a:as) (O.time e)
            (lane' // [(b, O.lane e b) | b <- cars])
            (ntg' // [((b, c), O.ntg e b c) | b <- cars, c <- cars])
            (ttc' // [((b, c), O.ttc e b c) | b <- cars, c <- cars])

   do_ a                    (State len as time' lane' ntg' ttc') =
      State (len+1) (a:as) time' lane' ntg' ttc'

   poss   = defaultPoss
   reward = defaultReward


instance State (Qty Double) where
   time (State _ _ t _     _    _   )     = wrap $ t
   lane (State _ _ _ lane' _    _   ) b   = lane' ! b
   ntg  (State _ _ _ _     ntg' _   ) b c = wrap $ ntg' ! (b, c)
   ttc  (State _ _ _ _     _    ttc') b c = wrap $ ttc' ! (b, c)


instance HistState (Qty Double) where
   history (State _   as _ _ _ _) = as
   histlen (State len _  _ _ _ _) = len

