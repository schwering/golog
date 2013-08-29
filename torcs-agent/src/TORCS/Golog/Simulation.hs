module TORCS.Golog.Simulation where

import TORCS.CarControl
import TORCS.CarState
import TORCS.Golog.Sensors
import TORCS.PhysicsUtil

trajectoryTo :: CarState -> Double -> Double -> Double
trajectoryTo cs yTo = sigf_ n . m2rel
   where yFrom     = trackPos cs
         yDiff     = yTo - yFrom
         yAlready  = sigf n x1
         sigf_ m x = (sigf m x - yAlready) * (yTo - yAlready) + yAlready
         sigf  m x = sig m x * yDiff + yFrom
         sigf' m x = sig' m x * yDiff
         maxSlope  = tan (maxSteeringAngle cs)
         curSlope  = tan (-1 * angle cs)
         n         = 0.5-- binSearch ((flip sigf') 0) (  1, 10) 0.01 maxSlope
         x1        = binSearch (sigf' n)        (-2, 0) 0.05 curSlope
         x2        = binSearch (sigf' n)        ( 0, 2) 0.05 0
         m2rel x   = (x - distRaced cs) / (trackWidth cs / 2) + x1

sig :: Floating a => a -> a -> a
sig n x = 1 / (1 + exp (-x / n))

sig' :: Floating a => a -> a -> a
sig' n x = sig n x * (1 - sig n x) / n

-- | Binary search on a function which must be monotonically nondecreasing on
-- the given interval for a wanted domain value within the interval.
-- Search aborts when the interval size falls below of the resolution parameter
-- @res@. Too small resolutions are dangerous due to floating point imprecision.
-- This could lead to infinite loops.
binSearch :: (Fractional a, Ord a, Num b, Ord b) => (a -> b) -> (a, a) -> a -> b -> a
binSearch f (lo',hi') res y | res <= 0      = error "binSearch: non-positive resolution"
                            | f lo' > f hi' = binSearch (((-1)*).f) (lo',hi') res (-y)
                            | otherwise     = bs (lo',hi')
   where bs (lo,hi) | hi - lo < res = x
                    | y >= f x      = binSearch f (x,hi) res y
                    | y <= f x      = binSearch f (lo,x) res y
                    | otherwise     = error $ "binSearch: impossible case"
            where x = (lo + hi) / 2

simulateState :: Double -> CarControl -> CarState -> CarState
simulateState t c s = s{angle         = angle s + angleT,
                        curLapTime    = curLapTime s + t,
                        distFromStart = distFromStart s + xR',
                        distRaced     = distRaced s + xR',
                        gear          = gearCmd c,
                        speedX        = speedX s + aX * t,
                        speedY        = speedY s + aY * t,
                        track         = trackT,
                        trackPos      = trackPos s + trackPosT }
   where -- Ackermann drive:
         xC'       = speedX s * t
         thetaW'   = tan (steerCmd c * steerLock) / wheelBase * xC'
         xW'       = cos thetaW' * xC'
         yW'       = sin thetaW' * xC'
         -- Car is already rotated by theta, so we project Ackermann drive onto
         -- the road coordinate system:
         theta     = -1 * angle s
         xR'       = xW' * cos theta - yW' * sin theta
         yR'       = xW' * sin theta + yW' * cos theta
         distT     = sqrt (xW'*xW' + yW'*yW')
         angleT    = angle s - thetaW'
         -- Progress the laser beams
         trackT    = map shortenBeams (zip beamOris rotatedBeams)
         rotatedBeams = rotateBeams (trackDist s) thetaW'
         shortenBeams (alpha, d) = d - distT * cos (thetaW' - alpha)
         trackPosT = yR' / trackWidth s
         -- Very naive model of acceleration with maximum acceleration 10 m/s^2,
         -- maximum deceleration 20 m/s^2:
         aX = accelCmd c * sin (speedX s / kmh2ms 300) * 10 - brakeCmd c * 20
         aY = yW' / t
         -- Old approach: use learnt data to predict next position.
         --thetaS            = atan2 (speedY s) (speedX s)
         --v                 = sqrt (speedX s ^ (2::Int) + speedY s ^ (2::Int))
         --distT             = v * cos (theta + yaw s) * t
         --trackT (alpha, d) = d - v * cos (theta - alpha) * t
         --trackPosT         = v * sin (theta + yaw s) * t / trackWidth s
         --angleT            = predictAngleDiff (steer c) t
         --vXT               = predictSpeedXDiff (speedX s) (accelCmd c) (brakeCmd c) (gearCmd c) t

