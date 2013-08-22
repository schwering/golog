{-# LANGUAGE TypeFamilies #-}

-- | Class for driver implementations.
-- An instance of the 'Driver' class should be handed to the 'run' function of
-- the "TORCS.Client" module.
module TORCS.Driver (Driver(..)) where

import TORCS.CarControl
import TORCS.CarState

-- | Class for SCR driver robots.
class Driver a where
   data Context a :: *

   -- | Name should be @\"SCR\"@ unless changed in the SCR TORCS robot.
   -- The first argument is dummy.
   name         :: a -> String

   -- | Orientation of the 19 range finders in degrees.
   -- The first argument is dummy.
   -- Default: the radians of the degrees in the range
   -- @[-90,-75,-60,-45,-30,20,15,10,5,0,5,10,15,20,30,45,60,75,90]@.
   beamOris     :: a -> [Double]

   -- | Returns the initial state.
   -- The first argument is dummy.
   initialState :: a -> IO (Context a)

   -- | Handler for incoming sensor readings which returns a new action.
   command      :: Context a -> CarState -> IO (Context a, CarControl)

   -- | Handler for the shutdown event.
   shutdown     :: Context a -> IO (Maybe (Context a))

   -- | Handler for the restart event.
   restart      :: Context a -> IO (Context a)

   name _     = "SCR"
   beamOris _ = [-90, -75, -60, -45, -30, -20, -15, -10, -5, 0, 5, 10, 15, 20, 30, 45, 60, 75, 90]
