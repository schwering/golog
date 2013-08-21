{-# LANGUAGE TypeFamilies #-}

-- | Class for driver implementations.
-- An instance of the 'Driver' class should be handed to the 'run' function of
-- the "TORCS.Client" module.
module TORCS.Driver (Driver(..), BeamOri(..), beamOriRad) where

import TORCS.CarControl
import TORCS.CarState
import TORCS.PhysicsUtil

data BeamOri =
   Neg90 | Neg75 | Neg60 | Neg45 | Neg30 | Neg20 | Neg15 | Neg10 | Neg5 | Zero |
   Pos5 | Pos10 | Pos15 | Pos20 | Pos30 | Pos45 | Pos60 | Pos75 | Pos90
   deriving (Show, Enum, Bounded, Eq, Ord)

beamOriRad :: BeamOri -> Double
beamOriRad Neg90 = deg2rad (-90)
beamOriRad Neg75 = deg2rad (-75)
beamOriRad Neg60 = deg2rad (-60)
beamOriRad Neg45 = deg2rad (-45)
beamOriRad Neg30 = deg2rad (-30)
beamOriRad Neg20 = deg2rad (-20)
beamOriRad Neg15 = deg2rad (-15)
beamOriRad Neg10 = deg2rad (-10)
beamOriRad Neg5  = deg2rad (-5)
beamOriRad Zero  = deg2rad 0
beamOriRad Pos5  = deg2rad 5
beamOriRad Pos10 = deg2rad 10
beamOriRad Pos15 = deg2rad 15
beamOriRad Pos20 = deg2rad 20
beamOriRad Pos30 = deg2rad 30
beamOriRad Pos45 = deg2rad 45
beamOriRad Pos60 = deg2rad 60
beamOriRad Pos75 = deg2rad 75
beamOriRad Pos90 = deg2rad 90

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
   beamOris _ = map beamOriRad [minBound .. maxBound]

