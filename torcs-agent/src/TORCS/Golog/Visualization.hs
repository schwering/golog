module TORCS.Golog.Visualization where

import Control.Concurrent.MSampleVar
import Data.IORef
import Golog.Interpreter
import TORCS.CarControl
import TORCS.CarState
import qualified TORCS.Golog.BAT.WarmUp as W
import TORCS.Golog.Sensors
import TORCS.Golog.Simulation
import TORCS.OpenGL

visualize :: MSampleVar (Sit W.A) -> IO ()
visualize sitVar = runOpenGL (shapes sitVar)

shapes :: MSampleVar (Sit W.A) -> IO [Shape]
shapes sitVar = do
   sit <- readSV sitVar
   return $ shapes' (W.cs sit) (W.cc sit)

shapes' :: CarState -> CarControl -> [Shape]
shapes' cs' cc' = replaceInfByOne $
                  map (translate (0,-0.8)) $
                  map (scale (1/20)) $
                  makeCar cs' cc' ++
                  --makeCar' 1 cs' cc' ++
                  beams cs' ++
                  trackLines cs'
   where track2draw cs y = (-y + trackPos cs) * trackWidth cs / 2
         -- Car and wheels.
         makeCar cs cc = [car, wheelFR, wheelFL, wheelRR, wheelRL]
            where yaw = -1 * angle cs
                  phi = steerCmd cc * steerLock
                  car = rotate yaw $ mkRect' blue (0,0) (fst fr - fst fl, snd fl - snd rl + 1)
                  wheelFR = rotate yaw $ translate fr $ rotate phi $ wheel
                  wheelFL = rotate yaw $ translate fl $ rotate phi $ wheel
                  wheelRR = rotate yaw $ translate rr $ wheel
                  wheelRL = rotate yaw $ translate rl $ wheel
                  wheel = mkRect' white (0,0) (0.3,0.55)
                  fl = (-0.840000,  1.220000)
                  fr = ( 0.840000,  1.220000)
                  rl = (-0.800000, -1.420000)
                  rr = ( 0.800000, -1.420000)
         -- Beams for track distance.
         beams cs = rawBeams ++ rotatedBeams
            where yaw = -1 * angle cs
                  rawBeams = map (rotate yaw) $
                             map (\(ori,d) -> rotate ori $ mkLine red (0,0) (0,d)) $
                             map (replaceInfBy 1000) $
                             zip beamOris (track' cs)
                  rotatedBeams = map (\(ori,d) -> rotate ori $ mkLine green (0,0) (0,d)) $
                             map (replaceInfBy 1000) $
                             zip beamOris (rotateBeams (trackDist cs) yaw)
            -- Track end and trajectory.
         trackLines cs = [trackLeft, trackRight, traj] ++
                         [mkLine white (0,-1/0) (0,1/0)] ++
                         [mkLine white (-1/0,0) (1/0,0)]
            where trackLeft = mkLine magenta (track2draw cs 1, -100) (track2draw cs 1, 100)
                  trackRight = mkLine magenta (track2draw cs (-1), -100) (track2draw cs (-1), 100)
                  traj = mkPolyline cyan $
                         map (\x -> (track2draw cs (t (distRaced cs + x)), x)) [0,1..100]
                  --t = trajectory s
                  t = trajectoryTo cs 1.0
         -- Predicted car in one second.
         makeCar' t cs cc = map (translate (x,y)) (makeCar cs1 cc)
            where cs1 = simulateState t cc cs
                  x   = track2draw cs (trackPos cs1 - trackPos cs)
                  y   = distRaced cs1 - distRaced cs

knrm, kred, kgrn, kyel, kblu, kmag, kcyn, kwht :: String
knrm = "\x1B[0m"
kred = "\x1B[31m"
kgrn = "\x1B[32m"
kyel = "\x1B[33m"
kblu = "\x1B[34m"
kmag = "\x1B[35m"
kcyn = "\x1B[36m"
kwht = "\x1B[37m"

