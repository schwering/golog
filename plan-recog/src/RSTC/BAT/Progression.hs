{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Basic action theory based on relative temporal measures using progression.

module RSTC.BAT.Progression (Qty, Wrapper(..),
                             Prim(..), NTGCat(..), TTCCat(..), State(..), HistState,
                             lookahead, ntgDiff, ttcDiff, quality, match,
                             bestAccel, ntgCats, ttcCats, nan,
                             inject, remove) where

import RSTC.Car
import RSTC.BAT.Base
import Golog.Interpreter
import Golog.Util
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
   data Sit (Prim (Qty Double)) = State {
      sitLen   :: Int,
      sitHist  :: [Prim (Qty Double)],
      sitPred  :: Maybe (Sit (Prim (Qty Double))),
      sitRew   :: Reward (Prim (Qty Double)),
      sitTime  :: Time Double,
      sitLanes :: Array Car Lane,
      sitNTGs  :: UArray (Car, Car) (NTG Double),
      sitTTCs  :: UArray (Car, Car) (TTC Double)
   }

   s0 = State {
      sitLen   = 0,
      sitHist  = [],
      sitPred  = Nothing,
      sitRew   = Reward (0, 0),
      sitTime  = 0,
      sitLanes = (array (minBound, maxBound) [(b, RightLane) | b <- [minBound..maxBound]]),
      sitNTGs  = (array (minBound, maxBound) [((b,c), nan) | b <- [minBound..maxBound], c <- [minBound..maxBound]]),
      sitTTCs  = (array (minBound, maxBound) [((b,c), nan) | b <- [minBound..maxBound], c <- [minBound..maxBound]])
   }

   do_ a@(Wait t) s = State {
      sitLen   = sitLen s + 1,
      sitHist  = a : sitHist s,
      sitPred  = Just s,
      sitRew   = sitRew s `addReward` actionReward a s,
      sitTime  = sitTime s + unwrap t,
      sitLanes = sitLanes s,
      sitNTGs  = sitNTGs s // [((b, c), unwrap $ tntg (ntg s) (ttc s) t b c) | b <- cars, c <- cars],
      sitTTCs  = sitTTCs s // [((b, c), unwrap $ tttc (ntg s) (ttc s) t b c) | b <- cars, c <- cars]
   }

   do_ a@(Accel b q) s = State {
      sitLen   = sitLen s + 1,
      sitHist  = a : sitHist s,
      sitPred  = Just s,
      sitRew   = sitRew s `addReward` actionReward a s,
      sitTime  = sitTime s,
      sitLanes = sitLanes s,
      sitNTGs  = sitNTGs s // ([((b, c), unwrap $ antg1 (ntg s) (ttc s) q b c) | c <- cars] ++
                               [((c, b), unwrap $ antg2 (ntg s) (ttc s) q c b) | c <- cars]),
      sitTTCs  = sitTTCs s // ([((b, c), unwrap $ attc1 (ntg s) (ttc s) q b c) | c <- cars] ++
                               [((c, b), unwrap $ attc2 (ntg s) (ttc s) q c b) | c <- cars])
   }

   do_ a@(LaneChange b l) s = State {
      sitLen   = sitLen s + 1,
      sitHist  = a : sitHist s,
      sitPred  = Just s,
      sitRew   = sitRew s `addReward` actionReward a s,
      sitTime  = sitTime s,
      sitLanes = sitLanes s // [(b, l)],
      sitNTGs  = sitNTGs s,
      sitTTCs  = sitTTCs s
   }

   do_ a@(Init e) s = State {
      sitLen   = sitLen s + 1,
      sitHist  = a : sitHist s,
      sitPred  = Just s,
      sitRew   = sitRew s `addReward` actionReward a s,
      sitTime  = O.time e,
      sitLanes = (sitLanes s // [(b, O.lane e b) | b <- cars]),
      sitNTGs  = (sitNTGs s // [((b, c), O.ntg e b c) | b <- cars, c <- cars]),
      sitTTCs  = (sitTTCs s // [((b, c), O.ttc e b c) | b <- cars, c <- cars])
   }

   do_ a s = State {
      sitLen   = sitLen s + 1,
      sitHist  = a : sitHist s,
      sitPred  = Just s,
      sitRew   = sitRew s `addReward` actionReward a s,
      sitTime  = sitTime s,
      sitLanes = sitLanes s,
      sitNTGs  = sitNTGs s,
      sitTTCs  = sitTTCs s
   }

   poss   = defaultPoss


addReward :: Reward (Prim (Qty Double)) -> (Double, Int) -> Reward (Prim (Qty Double))
addReward (Reward (r1,d1)) (r2,d2) = Reward (r1+r2, d1+d2)

instance DTBAT (Prim (Qty Double)) where
   newtype Reward (Prim (Qty Double)) = Reward (Double, Int)
      deriving (Eq, Ord)
   reward = sitRew


instance State (Qty Double) where
   time s     = wrap $ sitTime s
   lane s b   = sitLanes s ! b
   ntg  s b c = wrap $ sitNTGs s ! (b, c)
   ttc  s b c = wrap $ sitTTCs s ! (b, c)


instance HistBAT (Prim (Qty Double)) where
   history = sitHist
   sitlen  = sitLen
   predSit s' | sitLen s' == 0 = Nothing
              | otherwise      = Just (a, s)
      where (a:_)    = sitHist s'
            (Just s) = sitPred s'


instance HistState (Qty Double)

