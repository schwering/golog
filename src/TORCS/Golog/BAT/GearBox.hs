{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- | Low-level BAT for automatic transmission.
module TORCS.Golog.BAT.GearBox (A, mkS0, transmission, sense, fillCc) where

import Data.IORef
import Golog.Interpreter
import Golog.Macro
import TORCS.CarControl
import TORCS.CarState
import Debug.Trace

data A = GearDown | GearUp | Sense CarState | Test (Sit A -> Bool)

instance TestAction A where
   testAction = Test

instance BAT A where
   data Sit A = Sit {
      cs       :: CarState,
      cc       :: CarControl,
      ccRef    :: IORef CarControl,
      lastTime :: Double,
      sitLen   :: Int
   }

   s0 = Sit defaultState defaultControl undefined 0 0

   do_ a@GearDown  s = s{cc = (cc s){gearCmd = gear (cs s) - 1},
                         lastTime = curLapTime (cs s),
                         sitLen = sitLen s + 1}
   do_ a@GearUp    s = s{cc = (cc s){gearCmd = gear (cs s) + 1},
                         lastTime = curLapTime (cs s),
                         sitLen = sitLen s + 1}
   do_ (Sense cs') s = s{cs = cs',
                         sitLen = sitLen s + 1}
   do_ (Test _)    s = s

   poss (Test f) s = f s
   poss _        _ = True

mkS0 :: IORef CarControl -> Sit A
mkS0 ccRef' = s0{ccRef = ccRef'}

-- | Updates the given 'CarControl' with the portion controlled by this BAT.
-- That is, it sets the current 'gearCmd'.
fillCc :: Sit A -> CarControl -> CarControl
fillCc s cc' = cc'{gearCmd = gearCmd (cc s)}

instance IOBAT A IO where
   syncA a@GearDown s = do let s' = do_ a s
                           atomicModifyIORef' (ccRef s) (\cc' -> (cc'{gearCmd = gearCmd (cc s)}, ()))
                           return s'
   syncA a@GearUp   s = do let s' = do_ a s
                           atomicModifyIORef' (ccRef s) (\cc' -> (cc'{gearCmd = gearCmd (cc s)}, ()))
                           return s'
   syncA a@(Sense cs') s = return $ do_ a s
   syncA a@(Test _)    s = return $ do_ a s

-- | Performs a gear change if necessary. The necessity of gear changes depends
-- on the current RPM and is defined by a simple transmission table, i.e., if
-- certain thresholds of RPM are surpassed or fallen short of, one gear is
-- shifted up or down, respectively.
-- To thrashing, shifting up is only allowed after a certain time period has
-- passed since the last change.
transmission :: Prog A
transmission = if_ gearDownCond (then_
                  (prim GearDown))
               (else_ (if_ gearUpCond (then_
                  (prim GearUp))
               (else_ Nil)))
   where gearDownCond s = maybe False (\(lo,_) -> rpm (cs s) < lo) (rpms (gear (cs s)))
         gearUpCond   s = maybe False (\(_,hi) -> rpm (cs s) > hi && changeOk s) (rpms (gear (cs s)))
         changeOk s = curLapTime (cs s) - lastTime s > 1 -- min duration before shifting up
         rpms :: Int -> Maybe (Double, Double)
         rpms 0 = Just (-1/0,    1)
         rpms 1 = Just (   0, 5000)
         rpms 2 = Just (2500, 6000)
         rpms 3 = Just (3000, 6000)
         rpms 4 = Just (3000, 6500)
         rpms 5 = Just (3500, 7000)
         rpms 6 = Just (3500,  1/0)
         rpms _ = Nothing

-- | Informs the BAT about the new 'CarState'.
sense :: CarState -> Prog A
sense cs' = prim $ Sense cs'

