{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- | A low-level BAT for acceleration, deceleration and steering.
-- The numeric commands are filtered to keep the car stable.
module TORCS.Golog.BAT.Traction
  (Sit, accel, brake, brakeWet, steer, fillCc) where

import Data.IORef
import Golog.Interpreter
import Golog.Macro
import TORCS.CarControl
import TORCS.CarState
import TORCS.Golog.Sensors
import TORCS.PhysicsUtil

data A = Accel Double
       | Brake Double
       | Steer Double
       | Sense CarState
       | Test (Sit A -> Bool)

instance TestAction A where
   testAction = Test

instance BAT A where
   data Sit A = Sit {
      cs       :: CarState,
      cc       :: CarControl,
      ccRef    :: IORef CarControl,
      avgBrake :: Double,
      avgAccel :: Double,
      avgSteer :: Double
   }

   s0 = Sit defaultState defaultControl undefined 0 0 0

   do_ a@(Accel x) s = s{cc = updateCc a (cc s), avgAccel = (x + avgAccel s) / 2}
   do_ a@(Brake x) s = s{cc = updateCc a (cc s), avgBrake = (x + avgBrake s) / 2}
   do_ a@(Steer x) s = s{cc = updateCc a (cc s), avgSteer = (x + avgSteer s) / 2}
   do_ (Sense cs') s = s{cs = cs'}
   do_ (Test _)    s = s

   poss (Test f) s = f s
   poss _        _ = True

updateCc :: A -> CarControl -> CarControl
updateCc (Accel x) cc' = cc'{accelCmd = x}
updateCc (Brake x) cc' = cc'{brakeCmd = x}
updateCc (Steer x) cc' = cc'{steerCmd = x}
updateCc (Sense _) cc' = cc'
updateCc (Test _)  cc' = cc'

fillCc :: CarControl -> Sit A -> CarControl
fillCc cc' s = cc'{accelCmd = accelCmd (cc s),
                   brakeCmd = brakeCmd (cc s),
                   steerCmd = steerCmd (cc s)}

instance IOBAT A IO where
   syncA (Test _) s = return s
   syncA a s = do cc' <- readIORef (ccRef s)
                  writeIORef (ccRef s) (updateCc a cc')
                  return $ do_ a s

accel :: Double -> Prog A
accel x = primf action
   where action s | abs (currentSteeringAngle (cc s)) > maxSteeringAngle (cs s) = Accel (avgAccel s / 2)
                  | otherwise                                                   = Accel x

-- | Braking action where ABS only takes action if the slip is greater than 90%
-- and the car is steering non-negligibly.
brake :: Double -> Prog A
brake x = primf action
   where action s | x < kmh2ms 10                                          = Brake x
                  | wheelSlip (cs s) > 0.9 && abs (steerCmd (cc s)) > 0.05 = Brake (avgBrake s / 2)
                  | otherwise                                              = Brake x

-- | Braking action where ABS takes action if the slip is greater than 25%.
-- It seems as if in TORCS locked wheels brake very well, so this ABS may be too
-- defensive.
brakeWet :: Double -> Prog A
brakeWet x = primf action
   where action s | x < kmh2ms 10           = Brake x
                  | wheelSlip (cs s) > 0.25 = Brake (avgBrake s / 2)
                  | otherwise               = Brake x

-- | Steers to the left (@+1@) or to the left (@-1@).
-- If the steering is considered too strong with respect to the
-- 'maxSteeringAngle', the car is decelerated.
steer :: Double -> Prog A
steer x = primf action `Seq` ifThen tooMuchSteering slowDown
   where action s | tooMuchSteering s = Accel (avgAccel s / 2)
                  | otherwise         = Accel x
         tooMuchSteering s = abs x > maxSteeringAngle (cs s)
         slowDown = brake 1 `Seq` accel 0

