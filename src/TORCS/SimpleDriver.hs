{-# LANGUAGE TypeFamilies #-}

module TORCS.SimpleDriver where

import Data.Global
import Data.IORef
import Data.Maybe (fromMaybe)
import Golog.Interpreter
import TORCS.CarControl
import TORCS.CarState
import qualified TORCS.CarControl as Control
import qualified TORCS.CarState as State
import TORCS.Client
import TORCS.MessageParser

data Prim = Accel Double | Brake Double | Clutch Double | Gear Int |
            Steering Double | Focus Int | Meta Int | ReadSensors
   deriving Show

data SimpleDriver = SimpleDriver

instance BAT Prim where
   data Sit Prim = Sit CarState CarControl

   s0 = Sit state control
      where state = CarState {
               angle          = 0,
               curLapTime     = 0,
               damage         = 0,
               distFromStart  = 0,
               distRaced      = 0,
               State.focus    = replicate 5 (-1),
               fuel           = 0,
               State.gear     = 1,
               lastLapTime    = 0,
               opponents      = replicate 36 200,
               racePos        = 1,
               rpm            = 0,
               speedX         = 0,
               speedY         = 0,
               speedZ         = 0,
               track          = replicate 19 200,
               trackPos       = 0,
               wheelSpinVel   = replicate 4 0,
               z              = 0
            }
            control = CarControl {
               accel         = 0,
               brake         = 0,
               clutch        = 0,
               Control.gear  = 0,
               steering      = 0,
               Control.focus = 0,
               meta          = 0
            }

   do_ (Accel x)    (Sit state control) = Sit state control{accel = x}
   do_ (Brake x)    (Sit state control) = Sit state control{brake = x}
   do_ (Clutch x)   (Sit state control) = Sit state control{clutch = x}
   do_ (Gear x)     (Sit state control) = Sit state control{Control.gear = x}
   do_ (Steering x) (Sit state control) = Sit state control{steering = x}
   do_ (Focus x)    (Sit state control) = Sit state control{Control.focus = x}
   do_ (Meta x)     (Sit state control) = Sit state control{meta = x}
   do_ ReadSensors  s                   = s

   poss a s = True

-- | Global 'IORef' used to push the received 'CarState' into the situation.
-- That's really ugly because we cannot handle multiple drivers.
ref :: IORef (Maybe (CarState))
ref = declareIORef "some-cool-variable" Nothing

instance IOBAT Prim where
   syncA ReadSensors (Sit state0 control) =
      do state1 <- readIORef ref
         return $ Sit (fromMaybe state0 state1) control
   syncA a s =
      return $ do_ a s

instance Driver SimpleDriver where
   data State SimpleDriver = State (Sit Prim)
   initialState _    = do  writeIORef ref Nothing
                           return $ State s0
   command (State s) str = do putStrLn ("STATE: " ++ str)
                              let state = parseState str
                              writeIORef ref (Just state)
                              let cmd = stringify1 "accel" (0.75 :: Double)
                              return (State s, cmd)
   shutdown _        = do  putStrLn "SHUTDOWN"
   restart state     = do  putStrLn "RESTART"
                           return state

