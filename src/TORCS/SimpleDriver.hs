{-# LANGUAGE TypeFamilies #-}

module TORCS.SimpleDriver where

import Data.IORef
import Golog.Interpreter
import Text.Printf
import TORCS.CarControl
import TORCS.CarState
import qualified TORCS.CarControl as Control
import qualified TORCS.CarState as State
import TORCS.Client
import TORCS.MessageParser
import TORCS.PhysicsUtil

data Prim = Accel Double | Brake Double | Clutch Double | Gear Int |
            Steering Double | Focus Int | Meta Int | ReadSensors
   deriving Show

data SimpleDriver = SimpleDriver

instance BAT Prim where
   data Sit Prim = Sit {
      currentControl :: CarControl,
      currentState   :: CarState,
      sensedState    :: IORef CarState
   }

   s0 = Sit defaultControl defaultState undefined

   do_ (Accel x)    s = updateControl (\c -> c{accel = x}) s
   do_ (Brake x)    s = updateControl (\c -> c{brake = x}) s
   do_ (Clutch x)   s = updateControl (\c -> c{clutch = x}) s
   do_ (Gear x)     s = updateControl (\c -> c{Control.gear = x}) s
   do_ (Steering x) s = updateControl (\c -> c{steer = x}) s
   do_ (Focus x)    s = updateControl (\c -> c{Control.focus = x}) s
   do_ (Meta x)     s = updateControl (\c -> c{meta = x}) s
   do_ ReadSensors  s = s

   poss _ _ = True

updateControl :: (CarControl -> CarControl) -> Sit Prim -> Sit Prim
updateControl f (Sit control state sensings) = Sit (f control) state sensings

-- | Computes the time in seconds it takes for the car to reach the end of the
-- track in the direction to with the @i@th laser beam points according to
-- 'beamOris'.
--
-- Imagine the car's velocity in X and Y direction is a vector V and the
-- vector's angle from the positive X axis is @beta@.
-- Furthermore let there be a laser beam with angle @alpha@ indicate that the
-- track ends in @d@.
--
-- Since alpha is relative to the car's X axis, but the car's movement is
-- rotated by beta from the X axis, the car's movement is rotated by
-- @beta - alpha@ from the /beam/.
--
-- Therefore we project the car's velocity @v@ onto this beam by multiplying
-- @v * cos (beta - alpha)@.
-- Finally we divide the beam's length @d@ by this velocity to compute the time
-- it takes the car to travel the beam's distance in the beam's direction.
trackTime :: CarState -> [Double]
trackTime state = beamTimes
   where msX          = kmh2ms (speedX state)
         msY          = kmh2ms (speedY state)
         track'       = map (\d -> if d >= 200 then 1/0 else d) (track state)
         beta         = atan2 msY msX
         v            = sqrt (msX^(2::Int) + msY^(2::Int))
         beamOriDists = zip (beamOris SimpleDriver) track'
         beamTimes    = map (\(alpha,d) -> d / (v * cos (beta - alpha))) beamOriDists

instance IOBAT Prim where
   syncA ReadSensors s =
      do sensed <- readIORef (sensedState s)
         let s' = s{currentState = sensed}
         return s'
   syncA a s =
      return $ do_ a s

instance Driver SimpleDriver where
   data State SimpleDriver = State (Sit Prim)

   initialState _ = do  ref <- newIORef defaultState
                        return $ State s0{sensedState = ref}

   command (State s) str = do --putStrLn ("STATE: " ++ str)
                              let state = parseState str
                              _ <- printf "%.2f s  (%.2f m)    %.2f s  (%.2f m)    %.2f s  (%.2f m)\n"
                                   (trackTime state !!  0) (track state !!  0)
                                   (trackTime state !!  9) (track state !!  9)
                                   (trackTime state !! 18) (track state !! 18)
                              writeIORef (sensedState s) state
                              let cmd = stringify1 "accel" (0.75 :: Double)
                              return (State s, cmd)

   shutdown _ = do  putStrLn "SHUTDOWN"

   restart state = do  putStrLn "RESTART"
                       return state

