{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TORCS.Golog.Sensors where

import Data.List (find)
import Data.Maybe (fromMaybe)
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

beamOris :: [Double]
beamOris = map beamOriRad [minBound .. maxBound]

-- | Same as 'trackTime' but a functional interface.
trackTime :: CarState -> BeamOri -> Double
trackTime cs a = trackTime' cs !! fromEnum a

-- | Same as 'track'' but a functional interface.
trackDist :: CarState -> BeamOri -> Double
trackDist cs a = track' cs !! fromEnum a

-- | The track laser beams where the maximum reading @200@ is replaced by
-- infinity.
track' :: CarState -> [Double]
track' cs = map (\d -> if d >= 200 then 1/0 else d) (track cs)

-- | Computes the time in seconds it takes for the car to reach the end of the
-- track in the direction to with the @i@th laser beam points according to
-- 'beamOris'.
--
-- Imagine the car's velocity in X and Y direction is a vector V and the
-- vector's angle from the positive X axis is @theta@.
-- Furthermore let there be a laser beam with angle @alpha@ indicate that the
-- track ends in @d@.
--
-- Since alpha is relative to the car's X axis, but the car's movement is
-- rotated by theta from the X axis, the car's movement is rotated by
-- @theta - alpha@ from the /beam/.
--
-- Therefore we project the car's velocity @v@ onto this beam by multiplying
-- @v * cos (theta - alpha)@.
-- Finally we divide the beam's length @d@ by this velocity to compute the time
-- it takes the car to travel the beam's distance in the beam's direction.
trackTime' :: CarState -> [Double]
trackTime' cs = beamTimes
   where msX          = speedX cs
         msY          = speedY cs
         theta        = atan2 msY msX
         v            = sqrt (msX^(2::Int) + msY^(2::Int))
         beamOriDists = zip beamOris (track' cs)
         beamTimes    = map (\(alpha,d) -> d / (v * cos (theta - alpha))) beamOriDists

-- | Interpolates those two @beams@ which enclose the angle @theta@ and returns
-- the weighted average of these two beams. If there is no pair of enclosing
-- beams, @Nothing@ is returned.
projectBeam :: (BeamOri -> Double) -> Double -> Maybe Double
projectBeam beams theta = best >>= project
   where best = find cand [(lo, succ lo) | lo <- [minBound .. pred maxBound]]
         cand (lo,hi) = beamOriRad lo <= theta && theta <= beamOriRad hi
         project (lo,hi) | r == 0           = Just (beams lo)
                         | r == 1           = Just (beams hi)
                         | not (isNaN wavg) = Just wavg
                         | otherwise        = Nothing
            where r | hi == lo  = 1
                    | otherwise = (theta - beamOriRad lo) / (beamOriRad hi - beamOriRad lo)
                  wavg = (1 - r) * beams lo + r * beams hi

-- | Rotates the @beams@ anti-clockwise by angle @theta@. The typical use-case
-- is that the car rotated by clockwise angle @theta@ and we want to project the
-- @beams@ from the car's coordinate system to the road's coordinate system.
rotateBeams :: (BeamOri -> Double) -> Double -> [Double]
rotateBeams beams theta = map projectBeam' [minBound..maxBound]
   where projectBeam' alpha = fromMaybe (beams alpha) (projectedBeam alpha)
         projectedBeam alpha = projectBeam beams (beamOriRad alpha - theta)

-- | Computes the track width in meters based on the laser beam measurements.
trackWidth :: CarState -> Double
trackWidth cs = case (left, right) of
                     (Just l,  Just r)  -> l + r
                     (Just l,  Nothing) -> l / lpos
                     (Nothing, Just r)  -> r / rpos
                     (Nothing, Nothing) -> error "trackWidth: no beam"
   where gamma = -1 * angle cs
         left  = projectBeam (trackDist cs) (deg2rad   90  - gamma)
         right = projectBeam (trackDist cs) (deg2rad (-90) - gamma)
         pos   = (trackPos cs + 1) / 2
         lpos  = 1 - pos
         rpos  = pos

-- | Distance from front to rear axis in meters.
wheelBase :: Double
wheelBase = 2.64

-- | Maximum steering angle in radians.
steerLock :: Double
steerLock = deg2rad 21.0

-- | Wheel radius in the same order as wheelSpinVel.
-- Values copied from SCR's demo driver.
wheelRadius :: [Double]
wheelRadius = [0.3306, 0.3306, 0.3276, 0.3276]

-- | The average speed of the wheels.
avgWheelSpeed :: CarState -> Double
avgWheelSpeed cs = sum (map (uncurry (*)) (zip (wheelSpinVel cs) wheelRadius)) / 4

maxSteeringAngle :: CarState -> Double
maxSteeringAngle cs | speedX cs > 1 = 1 / speedX cs * steerLock
                    | otherwise     = steerLock

