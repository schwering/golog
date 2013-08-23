{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | A re-implementation of SCR's demo driver.
-- Left out clutching.
module TORCS.SimpleDriver where

--import Debug.Trace
import Text.Printf
import TORCS.CarControl
import TORCS.CarState
import TORCS.Client
import TORCS.PhysicsUtil

data SimpleDriver = SimpleDriver

instance Driver SimpleDriver where
   data Context SimpleDriver = Context {stuck :: Int}

   initialState _ =  return Context {stuck = 0}

   command s cs = do let (s', cc') = (drive s cs)
                     _ <- printf (kred ++
                                  "time = %.2f  " ++
                                  "pos = %.2f  " ++
                                  "angle = %.2f  " ++
                                  "vX = %.2f  " ++
                                  "vY = %.2f  " ++
                                  "track[-5] = %.2f s = %.2f m " ++
                                  "track[0] = %.2f s = %.2f m " ++
                                  "track[5] = %.2f s = %.2f m " ++
                                  knrm ++ "\n")
                                 (curLapTime cs)
                                 (trackPos cs)
                                 (deg $ rad2deg $ angle cs)
                                 (speedX cs)
                                 (speedY cs)
                                 (track cs !!  8) (track cs !!  8)
                                 (track cs !!  9) (track cs !!  9)
                                 (track cs !! 10) (track cs !! 10)
                     _ <- printf (kblu ++
                                  "accel = %.2f  " ++
                                  "brake = %.2f  " ++
                                  "gear = %d  " ++
                                  "steer = %.2f  " ++
                                  knrm ++ "\n")
                                 (accelCmd cc')
                                 (brakeCmd cc')
                                 (gearCmd cc')
                                 (steerCmd cc')
                     return (s', cc')

   shutdown _ = do   putStrLn "Bye bye!"
                     return Nothing

   restart cs = do   putStrLn "Restarting the race!"
                     return cs



-- Gear Changing Constants
gearUp = [5000, 6000, 6000, 6500, 7000, 0]
gearDown = [0, 2500, 3000, 3000, 3500, 3500]

-- Stuck constants
stuckTime = 25
stuckAngle = 0.523598775

-- Accel and Brake Constants
maxSpeedDist = 70
maxSpeed = kmh2ms 300
sin5 = 0.08716
cos5 = 0.99619

-- Steering constants
steerLock = 0.366519
steerSensitivityOffset = kmh2ms 80
wheelSensitivityCoeff = 1

-- ABS Filter Constants
wheelRadius = [0.3306, 0.3306, 0.3276, 0.3276]
absSlip = 2
absRange = 3
absMinSpeed = 3

-- Clutch constants
clutchMax = 0.5
clutchDelta = 0.05
clutchRange = 0.82
clutchDeltaTime = 0.02
clutchDeltaRaced = 10
clutchDec = 0.01
clutchMaxModifier = 1.3
clutchMaxTime = 1.5

getGear :: CarState -> Int
getGear cs | g < 1                             = 1
           | g < 6 && r >= gearUp !! (g - 1)   = g + 1
           | g > 1 && r <= gearDown !! (g - 1) = g - 1
           | otherwise                         = g
   where g = gear cs
         r = rpm cs

getSteer :: CarState -> Double
getSteer cs | speedX cs > steerSensitivityOffset = targetAngle / (steerLock * (speedX cs - steerSensitivityOffset) * 3.6 * wheelSensitivityCoeff)
            | otherwise                          = targetAngle / steerLock
   where targetAngle = angle cs - trackPos cs / 2

getAccel :: CarState -> Double
getAccel cs | -1 < trackPos cs && trackPos cs < 1 = 2 / (1 + exp ((speedX cs - targetSpeed) * 3.6)) - 1
            | otherwise = 0.3
   where rx = track cs !! 8
         c  = track cs !! 9
         sx = track cs !! 10
         targetSpeed | c > maxSpeedDist    = maxSpeed
                     | c >= rx && c >= sx  = maxSpeed
                     | rx > sx             = maxSpeed * (c * sinAngle rx / maxSpeedDist)
                     | otherwise           = maxSpeed * (c * sinAngle sx / maxSpeedDist)
         h = c * sin5
         b s = s - c * cos5
         sinAngle s = (b s) * (b s) / (h * h + (b s) * (b s))

drive :: Context SimpleDriver -> CarState -> (Context SimpleDriver, CarControl)
drive d cs | stuck d > stuckTime = (d', cc')
           | otherwise           = (d', cc'')
   where d' = if abs (angle cs) > stuckAngle
              then d{stuck = stuck d + 1}
              else d{stuck = 0}
         cc' = CarControl{steerCmd   = s,
                          gearCmd    = g,
                          accelCmd   = 1,
                          brakeCmd   = 0,
                          clutchCmd  = 0,
                          metaCmd    = 0,
                          focusCmd   = 0}
            where g = if angle cs * trackPos cs > 0 then 1 else -1
                  s = x * angle cs / steerLock
                  x = if angle cs * trackPos cs > 0 then 1 else -1
         cc'' = CarControl{steerCmd   = min 1 $ max (-1) $ getSteer cs,
                           gearCmd    = getGear cs,
                           accelCmd   = a,
                           brakeCmd   = b,
                           clutchCmd  = 0,
                           metaCmd    = 0,
                           focusCmd   = 0}
            where ab = getAccel cs
                  a = if ab > 0 then ab else 0
                  b = if ab > 0 then 0 else max 0 (filterABS cs (-1 * ab))

filterABS :: CarState -> Double -> Double
filterABS cs b | speedX cs < absMinSpeed = b
               | slip > absSlip          = b - (slip - absSlip) / absRange
               | otherwise               = b
   where slip' = sum $ map (uncurry (*)) $ zip (wheelSpinVel cs) wheelRadius
         slip  = speedX cs - slip' / 4

knrm, kred, kgrn, kyel, kblu, kmag, kcyn, kwht :: String
knrm = "\x1B[0m"
kred = "\x1B[31m"
kgrn = "\x1B[32m"
kyel = "\x1B[33m"
kblu = "\x1B[34m"
kmag = "\x1B[35m"
kcyn = "\x1B[36m"
kwht = "\x1B[37m"

