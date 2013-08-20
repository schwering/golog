{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

-- | A Golog-controlled SCR driver.
--
-- [High-level primitive actions:]
--
-- * @AntiDrift@: if @trackTime[+/- 90 deg]@ is below some threshold, we should
--   brake until @speedY@ falls below some threshold.
--
-- * @AntiSlip@: if @wheelSpinVel@ differ by some threshold, apply some brake.
--
-- * @AntiBlock@: if the fraction of @wheelSpinVel@ and @speedX@ falls below
--   some threshold while the brake is applied according to the current
--   @CarControl@, ease off the brakes.
--
-- * @Drive[Left|Center|Right]@: steer the car so that it goes to the
--   center/left/right of the road.
--
-- * @FollowBeam@: orients the car by the longest track beam.
--
-- [High-level programs:]
--
-- * @((AntiDrift | AntiSlip | AntiBlock) || (P1 | P2 | P3 | P4))*@ is the main
--   program.
--
-- * @P1 = trackTime[0 deg] = +infty? ; DriveCenter@ manages are on straight
--   segments.
--
-- * @P[23] = [Left|Right]CurveAhead(trackTime)? ; Drive[Right|Left]@ approaches
--   a curve.
--
-- * @P4 = FollowBeam@ drives through a curve.
--
-- [Low-level actions:]
--
-- The actions affect the 'CarControl' values: @accel@, @brake@, @gear@,
-- @steer@.
--
--
-- For that one high-level and one refined BAT is enough.
-- The @AntiDrift@ and @AntiSlip@ actions are preferred by assigning them a high
-- reward.
-- Only the low-level situation maintains a 'CarControl'.
--
-- One problem is that actions in the high-level BAT have a greater duration
-- than in the lower-level BAT, but still should be considered atomic from the
-- higher-level standpoint. E.g., @DriveCenter@ may take multiple game ticks
-- in the refined BAT. The problem is that the lower-level BAT needs to 'sync'ed
-- before the higher-level BAT has even executed.
--
-- [Random notes:]
--
-- * Do we need a low-level @Tick@ action?
--
-- * Where do we keep the 'CarState'?
--
-- * Where to put the @UpdateSensors@ action?
--
-- * Use a semaphore for the passing of new 'CarState's?
--
-- * Decouple Golog execution from SCR loop? If so, we could send actions in
--   'sync' via a channels plus semaphores.
--
-- * Use "STM" or "MVar" or "IORef"?
--
module TORCS.GologDriver where

import Control.Concurrent
import qualified Control.Concurrent.SSem as Sem
import Data.IORef
import Data.List (find)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Golog.Interpreter
import Golog.Macro
import Golog.Util
import Text.Printf
import System.Timeout (timeout)
import TORCS.CarControl
import TORCS.CarState
import qualified TORCS.CarControl as Control
import qualified TORCS.CarState as State
import TORCS.Client
import TORCS.PhysicsUtil
import Debug.Trace

data Prim1 = AntiDrift | AntiBlock |AntiSlip |
             ApproachLeft | DriveCenter | ApproachRight | FollowMaxBeam |
             Test1 (Sit1 -> Bool)

instance TestAction Prim1 where
   testAction = Test1

instance Show Prim1 where
   show AntiDrift     = "AntiDrift"
   show AntiBlock     = "AntiBlock"
   show AntiSlip      = "AntiSlip"
   show ApproachLeft  = "ApproachLeft"
   show DriveCenter   = "DriveCenter"
   show ApproachRight = "ApproachRight"
   show FollowMaxBeam = "FollowMaxBeam"
   show (Test1 _)     = "Test1 <...>"

data GologDriver = GologDriver
type Sit1 = Sit Prim1
type Sit2 = Sit Prim2
type Reward1 = Reward Prim1
type Reward2 = Reward Prim2
type Conf1 = ConfIO Prim1 IO
type Conf2 = ConfIO Prim2 IO
type Prog1 = Prog Prim1
type Prog2 = Prog Prim2

instance BAT Prim1 where
   data Sit Prim1 = Sit1 {
      rew1  :: Reward1,
      assoc :: Sit2
   }

   s0 = Sit1 0 s0

   do_ a@AntiDrift     s = s{rew1 = rew1 s + rew1A a s, assoc = sit $ refine a s (assoc s)}
   do_ a@AntiBlock     s = s{rew1 = rew1 s + rew1A a s, assoc = sit $ refine a s (assoc s)}
   do_ a@AntiSlip      s = s{rew1 = rew1 s + rew1A a s, assoc = sit $ refine a s (assoc s)}
   do_ a@ApproachLeft  s = s{rew1 = rew1 s + rew1A a s, assoc = sit $ refine a s (assoc s)}
   do_ a@DriveCenter   s = s{rew1 = rew1 s + rew1A a s, assoc = sit $ refine a s (assoc s)}
   do_ a@ApproachRight s = s{rew1 = rew1 s + rew1A a s, assoc = sit $ refine a s (assoc s)}
   do_ a@FollowMaxBeam s = s{rew1 = rew1 s + rew1A a s, assoc = sit $ refine a s (assoc s)}
   do_ a@(Test1 _)     s = s{rew1 = rew1 s + rew1A a s}

   poss AntiDrift     _ = False
   poss AntiBlock     _ = False
   poss AntiSlip      _ = False
   poss ApproachLeft  s = trackTime (currentState1 s) Zero < 5 &&
                          trackTime (currentState1 s) Neg5 >
                             trackTime (currentState1 s) Zero
   poss DriveCenter   _ = True-- trackTime (currentState1 s) Zero > 5
   poss ApproachRight s = trackTime (currentState1 s) Zero < 5 &&
                          trackTime (currentState1 s) Pos5 >
                             trackTime (currentState1 s) Zero
   poss FollowMaxBeam s = trackTime (currentState1 s) Zero < 3
   poss (Test1 f)  s = f s


instance DTBAT Prim1 where
   newtype Reward Prim1 = Reward1 Double
      deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)
   reward = rew1

instance IOBAT Prim1 IO where
   syncA a s =
      do putStrLn $ show a
         c2 <- dooSync' (refine a s (assoc s))
         if isNothing c2
            then fail $ "syncA: refinement of " ++ show a ++ " failed"
            else return ()
         --putStrLn $ show a ++ " done"
         return $ (do_ a s){assoc = sit (fromJust c2)}

rew1A :: Prim1 -> Sit1 -> Reward1
rew1A AntiDrift     _ = 1000
rew1A AntiBlock     _ = 500
rew1A AntiSlip      _ = 100
rew1A ApproachLeft  _ = 0
rew1A DriveCenter   _ = 1
rew1A ApproachRight _ = 0
rew1A FollowMaxBeam _ = 0
rew1A (Test1 _)     _ = 0

currentState1 :: Sit1 -> CarState
currentState1 = currentState2 . assoc

refine :: Prim1 -> Sit1 -> Sit2 -> Conf2
refine AntiDrift     _ s2 = refineProg Nil s2
refine AntiBlock     _ s2 = refineProg Nil s2
refine AntiSlip      _ s2 = refineProg Nil s2
refine ApproachLeft  _ s2 = refineProg (prim $ SetTrajectory   0.8 ) s2
refine DriveCenter   _ s2 = refineProg (prim $ SetTrajectory   0   ) s2
refine ApproachRight _ s2 = refineProg (prim $ SetTrajectory (-0.8)) s2
refine FollowMaxBeam _ s2 = refineProg (steerAngle longestBeam) s2
   where longestBeam = beamOriRad $ snd $ maximum $
                       map (\ori -> (trackTime (currentState2 s2) ori, ori))
                       [minBound .. maxBound]
refine (Test1 _)     _ _  = error "Test1 is not refinable"

refineProg :: Prog2 -> Sit2 -> Conf2
refineProg p' s = treeDTIO 3 p s
   where p = p' `Seq` transmission `Seq` acceleration `Seq` steerTrajectory `Seq` tick

data Prim2 = Accel Double | Brake Double | Clutch Double | Gear Int |
             Steer Double | Focus Int | Meta Int |
             SetTrajectory Double | Tick (Maybe CarState) |
             Test2 (Sit2 -> Bool) | Bounty (Sit2 -> Reward2)

instance TestAction Prim2 where
   testAction = Test2

instance Show Prim2 where
   show (Accel x)         = "Accel " ++ show x
   show (Brake x)         = "Brake " ++ show x
   show (Clutch x)        = "Clutch " ++ show x
   show (Gear x)          = "Gear " ++ show x
   show (Steer x)         = "Steer " ++ show x
   show (Focus x)         = "Focus " ++ show x
   show (Meta x)          = "Meta " ++ show x
   show (SetTrajectory x) = "SetTrajectory " ++ show x
   show (Tick x)          = "Tick " ++ show x
   show (Test2 _)         = "Test2 <...>"
   show (Bounty _)        = "Bounty <...>"

instance BAT Prim2 where
   data Sit Prim2 = Sit2 {
      currentState2   :: CarState,
      currentControl2 :: CarControl,
      sensedState     :: IORef CarState,
      newControl      :: IORef CarControl,
      ticks           :: Sem.SSem,
      rew2            :: Reward2,
      trajectory      :: Double -> Double,
      alreadyAccel    :: Bool,
      alreadyBrake    :: Bool,
      alreadyGear     :: Bool,
      alreadySteer    :: Bool
   }

   s0 = Sit2 defaultState defaultControl
             undefined undefined undefined
             0 (const 0) False False False False

   do_ a@(Accel x)         s = modControl (\y -> y{accel = x}) s{alreadyAccel = True, rew2 = rew2 s + rew2A a s}
   do_ a@(Brake x)         s = modControl (\y -> y{brake = x}) s{alreadyBrake = True, rew2 = rew2 s + rew2A a s}
   do_ a@(Clutch x)        s = modControl (\y -> y{clutch = x}) s{rew2 = rew2 s + rew2A a s}
   do_ a@(Gear x)          s = modControl (\y -> y{Control.gear = x}) s{alreadyGear = True, rew2 = rew2 s + rew2A a s}
   do_ a@(Steer x)         s = modControl (\y -> y{steer = x}) s{alreadySteer = True, rew2 = rew2 s + rew2A a s}
   do_ a@(Focus x)         s = modControl (\y -> y{Control.focus = x}) s{rew2 = rew2 s + rew2A a s}
   do_ a@(Meta x)          s = modControl (\y -> y{meta = x}) s{rew2 = rew2 s + rew2A a s}
   do_ a@(SetTrajectory x) s = s{trajectory = const x,
                                 rew2 = rew2 s + rew2A a s}
   do_ a@(Tick (Just x))   s = s{currentState2 = x,
                                 alreadyAccel  = False,
                                 alreadyBrake  = False,
                                 alreadyGear   = False,
                                 alreadySteer  = False,
                                 rew2 = rew2 s + rew2A a s}
   do_ a@(Tick Nothing)    s = modState (simulateState tickDurSec (currentControl2 s))
                               s{alreadyAccel = False,
                                 alreadyBrake = False,
                                 alreadyGear  = False,
                                 alreadySteer = False,
                                 rew2 = rew2 s + rew2A a s}
   do_ a@(Test2 _)         s = s{rew2 = rew2 s + rew2A a s}
   do_ a@(Bounty _)        s = s{rew2 = rew2 s + rew2A a s}

   poss (Accel _)         s = True || not (alreadyAccel s) -- a < rpm (currentState2 s) / 7500
   poss (Brake _)         _ = True
   poss (Clutch _)        _ = True
   poss (Gear _)          _ = True -- && abs (n - State.gear (currentState2 s)) <= 1
   poss (Steer _)         s = not (alreadySteer s) -- (speedX (currentState2 s)) > 1
   poss (Focus _)         _ = True
   poss (SetTrajectory _) _ = True
   poss (Meta _)          _ = True
   poss (Tick _)          _ = True
   poss (Test2 f)         s = f s
   poss (Bounty _)        _ = True

instance DTBAT Prim2 where
   newtype Reward Prim2 = Reward2 Double
      deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)
   reward = rew2

instance IOBAT Prim2 IO where
   syncA (Tick Nothing) s =
      do _ <- printf (kblu ++
                      "accel = %.2f  " ++
                      "brake = %.2f  " ++
                      "gear = %d  " ++
                      "steer = %.2f  " ++
                      knrm ++ "\n")
                     (accel (currentControl2 s))
                     (brake (currentControl2 s))
                     (Control.gear (currentControl2 s))
                     (steer (currentControl2 s))
         Sem.wait (ticks s)
         sensed <- readIORef (sensedState s)
         let s' = do_ (Tick (Just sensed)) s
         return s'
   syncA (Tick (Just _)) _ =
         error "syncA: Tick (Just _) already synced"
   syncA a s =
      do let s' = do_ a s
         writeIORef (newControl s) (currentControl2 s)
         return s'

rew2A :: Prim2 -> Sit2 -> Reward2
rew2A (Accel _)         s = if alreadyAccel s then -1 else -0.01
rew2A (Brake _)         s = if alreadyBrake s then -1 else -0.01
rew2A (Clutch _)        _ = 0
rew2A (Gear _)          s = if alreadyGear  s then -1 else -0.01
rew2A (Steer _)         s = if alreadySteer s then -1 else -0.01
rew2A (Focus _)         _ = 0
rew2A (Meta _)          _ = 0
rew2A (SetTrajectory _) _ = 0
rew2A (Tick _)          s = Reward2 $ x + 10 * (1 - abs (goal - trackPos cs))
   where cs   = currentState2 s
         x    = distRaced cs
         goal = trajectory s x
rew2A (Test2 _)         _ = 0
rew2A (Bounty f)        s = f s

modControl :: (CarControl -> CarControl) -> Sit2 -> Sit2
modControl f s = s{currentControl2 = f (currentControl2 s)}

modState :: (CarState -> CarState) -> Sit2 -> Sit2
modState f s = s{currentState2 = f (currentState2 s)}

tickDurSec :: Double
tickDurSec = 10 / 1000

tickDurMicroSec :: Int
tickDurMicroSec = 10 * 1000

tick :: Prog2
tick = prim (Tick Nothing)

bounty :: (Sit2 -> Reward2) -> Prog2
bounty = prim . Bounty

transmission :: Prog2
transmission = primf (\s -> Gear (case lookup (g s) rpms of
                                       Just (lo,hi) | rpm' s < lo -> d s
                                                    | rpm' s > hi -> u s
                                       _                          -> g s))
   where rpm' = State.rpm . currentState2
         g    = State.gear . currentState2
         u    = (+1) . g
         d    = (+ (-1)) . g
         rpms = [(0, (-1/0,    1)),
                 (1, (   0, 5000)),
                 (2, (2500, 6000)),
                 (3, (3000, 6000)),
                 (4, (3000, 6500)),
                 (5, (3500, 7000)),
                 (6, (3500,  1/0))]

acceleration :: Prog2
acceleration = atomic $
               primf (\s -> action (currentState2 s)) `Seq`
               primf (\s -> if alreadyAccel s then Brake 0 else Accel 0)
   where action state = let x = param state
                        in if x < 0 then Brake (-x) else Accel x
         param  state = if speedX state < 10 then 0.5 else param' state
         param' state = max (-1) $ min 1 $ (1 - 1 / beam)
            where msX   = speedX state
                  msY   = speedY state
                  theta = atan2 msY msX
                  beam  = fromMaybe (1/0) (projectBeam (trackTime state) theta)

steerAngle :: Double -> Prog2
steerAngle lock  = star action `Seq` success
   where action  = primf (steerAngleAction lock)
         success = test (\s -> abs (angle (currentState2 s) - lock) < deg2rad 3)

steerAngleAction :: Double -> Sit2 -> Prim2
steerAngleAction lock s = Steer lock'
   where lock' = lock / (speedX (currentState2 s))**0.5
         --state = currentState2 s
         --lock' = if v > steerSensitivityOffset
         --        then lock / (steerLock * 3.6 * (v - steerSensitivityOffset))
         --        else lock * abs lock
         --v    = speedX state
         --steerSensitivityOffset = kmh2ms 80

steerTrackPos :: Double -> Prog2
steerTrackPos pos = star action `Seq` success
   where action      = primf (\s -> steerAngleAction (lock (currentState2 s)) s)
         lock state  = maxAngle * (diff state) / 2
         diff state  = pos - trackPos state
         maxAngle    = deg2rad 45
         success     = bounty (\s -> Reward2 $ 1 / abs (trackPos (currentState2 s) - pos))

steerTrajectory :: Prog2
steerTrajectory = primf (\s -> steerAngleAction (lock s) s)
   where lock s   = maxAngle * (diff s) / 2
         curPos s = trackPos (currentState2 s)
         tgtPos s = trajectory s (distRaced (currentState2 s))
         diff s   = tgtPos s - curPos s
         maxAngle = deg2rad 45

sig :: Floating a => a -> a
sig x = 1 / (1 + exp (-x))

sig' :: Floating a => a -> a
sig' x = sig x * (1 - sig x)

{-
findSigStart :: Double -> Double
findSigStart rad
   where y  = tan rad
         yp = map sig' [0, -0.
-}

-- | Binary search on a function which must be monotonically nondecreasing on
-- the given interval for a wanted domain value within the interval.
-- Search aborts when the interval size falls below of the resolution parameter
-- @res@. Too small resolutions are dangerous due to floating point imprecision.
-- This could lead to infinite loops.
binSearch :: (Show a, Show b, Fractional a, Ord a, Num b, Ord b) => (a -> b) -> (a, a) -> a -> b -> a
binSearch f (lo',hi') res y | res <= 0      = error "binSearch: non-positive resolution"
                            | f lo' > f hi' = binSearch (((-1)*).f) (lo',hi') res (-y)
                            | otherwise     = bs (lo',hi')
   where bs (lo,hi) | hi - lo < res = x
                    | y >= f x      = binSearch f (x,hi) res y
                    | y <= f x      = binSearch f (lo,x) res y
                    | otherwise     = error $ "binSearch: impossible case " ++ show x ++ " " ++ show (f x)
            where x = (lo + hi) / 2

gologAgent :: IORef CarState -> IORef CarControl -> Sem.SSem -> IO ()
gologAgent stateRef controlRef tickSem = loop conf
   where loop c = do let cs = trans c
                     if null cs
                        then do putStrLn "EOP"
                        else do let c' = head cs
                                c'' <- sync c'
                                loop c''
         conf   = treeDTIO 3 p s01 :: Conf1
         p      = star $ Nondet $ map prim [AntiDrift, AntiBlock, AntiSlip,
                                            ApproachLeft, DriveCenter,
                                            ApproachRight, FollowMaxBeam]
         s01    = s0{assoc = s02}
         s02    = s0{sensedState = stateRef,
                     newControl  = controlRef,
                     ticks       = tickSem}

{-
-- | Puts a new value into an 'MVar' after taking the old value if there is one.
-- The idea is that this is a non-blocking (in contrast to 'putMVar') write
-- which is always successful (in contrast to 'tryPutMVar'). This only holds if
-- this thread is the only writer of the 'MVar'.
overwriteMVar :: MVar a -> a -> IO ()
overwriteMVar mvar x = tryTakeMVar mvar >> putMVar mvar x
-}

-- | Same as 'trackTime' but a functional interface.
trackTime :: CarState -> BeamOri -> Double
trackTime state a = trackTime' state !! fromEnum a

-- | Same as 'track'' but a functional interface.
trackDist :: CarState -> BeamOri -> Double
trackDist state a = track' state !! fromEnum a

-- | The track laser beams where the maximum reading @200@ is replaced by
-- infinity.
track' :: CarState -> [Double]
track' state = map (\d -> if d >= 200 then 1/0 else d) (track state)

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
trackTime' state = beamTimes
   where msX          = speedX state
         msY          = speedY state
         theta        = atan2 msY msX
         v            = sqrt (msX^(2::Int) + msY^(2::Int))
         beamOriDists = zip (beamOris GologDriver) (track' state)
         beamTimes    = map (\(alpha,d) -> d / (v * cos (theta - alpha))) beamOriDists

-- | Interpolates those two @beams@ which enclose the angle @theta@ and returns
-- the weighted average of these two beams. If there is no pair of enclosing
-- beams, @Nothing@ is returned.
projectBeam :: (BeamOri -> Double) -> Double -> Maybe Double
projectBeam beams theta = fmap project best
   where best = find cand [(lo, succ lo) | lo <- [minBound .. pred maxBound]]
         cand (lo,hi) = beamOriRad lo <= theta && theta <= beamOriRad hi
         project (lo,hi) = (if r /= 1 then x else 0) + (if r /= 0 then y else 0)
            where r = if hi == lo then 1
                      else (theta - beamOriRad lo) / (beamOriRad hi - beamOriRad lo)
                  x = (1-r) * beams lo
                  y = r * beams hi

-- | Rotates the @beams@ anti-clockwise by angle @theta@. The typical use-case
-- is that the car rotated by clockwise angle @theta@ and we want to project the
-- @beams@ from the car's coordinate system to the road's coordinate system.
rotateBeams :: (BeamOri -> Double) -> Double -> [Double]
rotateBeams beams theta = map projectBeam' [minBound..maxBound]
   where projectBeam' alpha = fromMaybe (beams alpha) (projectedBeam alpha)
         projectedBeam alpha = projectBeam beams (beamOriRad alpha - theta)

-- | Computes the track width in meters based on the laser beam measurements.
trackWidth :: CarState -> Double
trackWidth state = case (left, right) of
                        (Just l,  Just r)  -> l + r
                        (Just l,  Nothing) -> l / lpos
                        (Nothing, Just r)  -> r / rpos
                        (Nothing, Nothing) -> error "trackWidth: no beam"
   where gamma = -1 * angle state
         left  = projectBeam (trackDist state) (deg2rad   90  - gamma)
         right = projectBeam (trackDist state) (deg2rad (-90) - gamma)
         pos   = (trackPos state + 1) / 2
         lpos  = 1 - pos
         rpos  = pos

simulateState :: Double -> CarControl -> CarState -> CarState
simulateState t c s = s{angle         = angle s + angleT,
                        curLapTime    = curLapTime s + t,
                        distFromStart = distFromStart s + xR',
                        distRaced     = distRaced s + xR',
                        State.gear    = Control.gear c,
                        speedX        = speedX s + aX * t,
                        speedY        = speedY s + aY * t,
                        track         = trackT,
                        trackPos      = trackPos s + trackPosT }
   where -- Ackermann drive:
         xC'       = speedX s * t
         thetaW'   = tan (steer c * steerLock) / wheelBase * xC'
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
         trackT    = map shortenBeams (zip (beamOris GologDriver) rotatedBeams)
         rotatedBeams = rotateBeams (trackDist s) thetaW'
         shortenBeams (alpha, d) = d - distT * cos (thetaW' - alpha)
         trackPosT = yR' / trackWidth s
         -- Very naive model of acceleration with maximum acceleration 10 m/s^2,
         -- maximum deceleration 20 m/s^2:
         aX = accel c * sin (speedX s / kmh2ms 300) * 10 - brake c * 20
         aY = yW' / t
         -- Old approach: use learnt data to predict next position.
         --thetaS            = atan2 (speedY s) (speedX s)
         --v                 = sqrt (speedX s ^ (2::Int) + speedY s ^ (2::Int))
         --distT             = v * cos (theta + yaw s) * t
         --trackT (alpha, d) = d - v * cos (theta - alpha) * t
         --trackPosT         = v * sin (theta + yaw s) * t / trackWidth s
         --angleT            = predictAngleDiff (steer c) t
         --vXT               = predictSpeedXDiff (speedX s) (accel c) (brake c) (Control.gear c) t

-- | Distance from front to rear axis in meters.
wheelBase :: Double
wheelBase = 2.64

-- | Maximum steering angle in radians.
steerLock :: Double
steerLock = deg2rad 21.0

instance Driver GologDriver where
   data Context GologDriver = Context (IORef CarState) (IORef CarControl)
                                       Sem.SSem ThreadId

   initialState _ = do  stateRef <- newIORef defaultState
                        controlRef <- newIORef defaultControl
                        tickSem <- Sem.new 0
                        gologThread <- forkIO $
                                       gologAgent stateRef controlRef tickSem
                        return (Context stateRef controlRef tickSem gologThread)

   command ctx@(Context stateRef controlRef tickSem _) state =
      do writeIORef stateRef state
         --putStrLn $ show state
         Sem.signal tickSem
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
               (curLapTime state)
               (trackPos state)
               (deg $ rad2deg $ angle state)
               (speedX state)
               (speedY state)
               (trackTime state Neg5) (trackDist state Neg5)
               (trackTime state Zero)  (trackDist state Zero)
               (trackTime state Pos5) (trackDist state Pos5)
         --putStrLn (kmag ++ show (track state) ++ knrm)
         control <- timeout (tickDurMicroSec) $ readIORef controlRef
         --putStrLn $ show control
         return (ctx, fromMaybe defaultControl control)

   shutdown ctx =
      do putStrLn "SHUTDOWN"
         --killThread gologThread
         ctx' <- restart ctx
         return (Just ctx')

   restart (Context stateRef controlRef _ gologThread) =
      do putStrLn "RESTART"
         killThread gologThread
         tickSem <- Sem.new 0
         newGologThread <- forkIO $
                           gologAgent stateRef controlRef tickSem
         return (Context stateRef controlRef tickSem newGologThread)

knrm, kred, kgrn, kyel, kblu, kmag, kcyn, kwht :: String
knrm = "\x1B[0m"
kred = "\x1B[31m"
kgrn = "\x1B[32m"
kyel = "\x1B[33m"
kblu = "\x1B[34m"
kmag = "\x1B[35m"
kcyn = "\x1B[36m"
kwht = "\x1B[37m"

