{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module TORCS.Golog.Control where

import qualified Control.Concurrent.SSem as Sem
import Data.IORef
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Golog.Interpreter
import Golog.Macro
import Golog.Util
import TORCS.CarControl
import TORCS.CarState
import TORCS.Golog.Sensors
import TORCS.Golog.Simulation
import TORCS.PhysicsUtil

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
      do --putStrLn $ show a
         c2 <- dooIO (const Online) (refine a s (assoc s))
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
refine DriveCenter   _ s2 = refineProg (prim $ SetTrajectory   0.0 ) s2
refine ApproachRight _ s2 = refineProg (prim $ SetTrajectory (-0.8)) s2
refine FollowMaxBeam _ s2 = refineProg (steerAngle longestBeam) s2
   where longestBeam = beamOriRad $ snd $ maximum $
                       map (\ori -> (trackTime (currentState2 s2) ori, ori))
                       [minBound .. maxBound]
refine (Test1 _)     _ _  = error "Test1 is not refinable"

refineProg :: Prog2 -> Sit2 -> Conf2
refineProg p' s = treeDTIO 3 p s
   where p = p' `Seq` transmission `Seq` steerTrajectory `Seq` acceleration `Seq` tick

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

   do_ a@(Accel x)         s = modControl (\y -> y{accelCmd = x}) s{alreadyAccel = True, rew2 = rew2 s + rew2A a s}
   do_ a@(Brake x)         s = modControl (\y -> y{brakeCmd = x}) s{alreadyBrake = True, rew2 = rew2 s + rew2A a s}
   do_ a@(Clutch x)        s = modControl (\y -> y{clutchCmd = x}) s{rew2 = rew2 s + rew2A a s}
   do_ a@(Gear x)          s = modControl (\y -> y{gearCmd = x}) s{alreadyGear = True, rew2 = rew2 s + rew2A a s}
   do_ a@(Steer x)         s = modControl (\y -> y{steerCmd = x}) s{alreadySteer = True, rew2 = rew2 s + rew2A a s}
   do_ a@(Focus x)         s = modControl (\y -> y{focusCmd = x}) s{rew2 = rew2 s + rew2A a s}
   do_ a@(Meta x)          s = modControl (\y -> y{metaCmd = x}) s{rew2 = rew2 s + rew2A a s}
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
   poss (Steer _)         _ = True -- not (alreadySteer s) -- (speedX (currentState2 s)) > 1
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
      do Sem.wait (ticks s)
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
rew2A (Accel x)         _ = Reward2 $ x -- if alreadyAccel s then -1 else -0.01
rew2A (Brake x)         _ = Reward2 $ -x -- if alreadyBrake s then -1 else -0.01
rew2A (Clutch _)        _ = 0
rew2A (Gear _)          _ = 0 -- if alreadyGear  s then -1 else -0.01
rew2A (Steer _)         _ = 0 -- if alreadySteer s then -1 else -0.01
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

tick :: Prog2
tick = prim (Tick Nothing)

bounty :: (Sit2 -> Reward2) -> Prog2
bounty = prim . Bounty

transmission :: Prog2
transmission = primf (\s -> Gear (case lookup (g s) rpms of
                                       Just (lo,hi) | rpm' s < lo -> d s
                                                    | rpm' s > hi -> u s
                                       _                          -> g s))
   where rpm' = rpm . currentState2
         g    = gear . currentState2
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
               primf (\s -> action s) `Seq`
               primf (\s -> if alreadyAccel s then Brake 0 else Accel 0)
   where action s | param < 0 = Brake (filterABS cs (-param))
                  | otherwise = Accel param
            where cs = currentState2 s
                  cc = currentControl2 s
                  param | speedX cs < 5 = 1
                        | otherwise     = max (-1) $ min 1 $ filterSteer $ filterDiff param'
                  param' = (1 - 1 / beam)
                     where msX = speedX cs
                           msY = speedY cs
                           theta = atan2 msY msX
                           beam  = fromMaybe (1/0) (projectBeam (trackTime cs) theta)
                  filterDiff x = x - diff / 4
                     where diff = abs (trackPos cs - wantedTrackPos s)
                  filterSteer x = x - steerCmd cc

wantedTrackPos :: Sit2 -> Double
wantedTrackPos s = trajectory s (distRaced (currentState2 s))

filterABS :: CarState -> Double -> Double
filterABS cs b | v < absMinSpeed = b
               | slip > absSlip  = max 0 (b - (slip - absSlip) / absRange)
               | otherwise       = b
   where msX = speedX cs
         msY = speedY cs
         v = sqrt (msX*msX + msY*msY)
         slip = v - avgWheelSpeed cs

steerAngle :: Double -> Prog2
steerAngle lock  = star action `Seq` success
   where action  = primf (steerAngleAction lock)
         success = test (\s -> abs (angle (currentState2 s) - lock) < deg2rad 3)

steerAngleAction :: Double -> Sit2 -> Prim2
steerAngleAction lock s = Steer lock'
   where lock' = lock / (speedX (currentState2 s))**0.5
         --cs = currentState2 s
         --lock' = if v > steerSensitivityOffset
         --        then lock / (steerLock * 3.6 * (v - steerSensitivityOffset))
         --        else lock * abs lock
         --v    = speedX cs
         --steerSensitivityOffset = kmh2ms 80

steerTrackPos :: Double -> Prog2
steerTrackPos pos = star action `Seq` success
   where action   = primf (\s -> steerAngleAction (lock (currentState2 s)) s)
         lock cs  = maxAngle * (diff cs) / 2
         diff cs  = pos - trackPos cs
         maxAngle = deg2rad 45
         success  = bounty (\s -> Reward2 $ 1 / abs (trackPos (currentState2 s) - pos))

steerTrajectory :: Prog2
steerTrajectory = primf (\s -> Steer (lock s))
   where deltaX  = 1
         t s x   = trajectory s (distRaced (currentState2 s) + x) * trackWidth (currentState2 s)
         p s     = trackPos (currentState2 s) * trackWidth (currentState2 s)
         theta s = atan2 (t s deltaX - p s) deltaX
         v s     = speedX (currentState2 s)
         phi s   = atan2 (theta s * wheelBase) (v s)
         lock s  = phi s / steerLock

steerTrajectory' :: Prog2
steerTrajectory' = primf (\s -> Steer (lock s))
   where lock s   = steerCmd (currentControl2 s) + signum (diff s) * 0.001
         curPos s = trackPos (currentState2 s)
         tgtPos s = trajectory s (distRaced (currentState2 s))
         diff s   = tgtPos s - curPos s

gologAgent :: IORef CarState -> IORef CarControl -> Sem.SSem -> IO ()
gologAgent csRef ccRef tickSem = loop conf
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
         s02    = s0{sensedState = csRef,
                     newControl  = ccRef,
                     ticks       = tickSem}

-- | ABS constants.
absSlip, absRange, absMinSpeed :: Double
absSlip = 2
absRange = 3
absMinSpeed = 3

