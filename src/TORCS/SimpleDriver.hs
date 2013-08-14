{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

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
module TORCS.SimpleDriver where

import Control.Concurrent
import qualified Control.Concurrent.SSem as Sem
import Data.IORef
import Data.List (find)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Golog.Interpreter
import Golog.Macro
import Golog.Util
--import Text.Printf
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
             Test1 (Sit Prim1 -> Bool)

data Prim2 = Accel Double | Brake Double | Clutch Double | Gear Int |
             Steer Double | Focus Int | Meta Int | Tick (Maybe CarState) |
             Test2 (Sit Prim2 -> Bool)

instance TestAction Prim1 where
   testAction = Test1

instance TestAction Prim2 where
   testAction = Test2

instance Show Prim1 where
   show AntiDrift     = "AntiDrift"
   show AntiBlock     = "AntiBlock"
   show AntiSlip      = "AntiSlip"
   show ApproachLeft  = "ApproachLeft"
   show DriveCenter   = "DriveCenter"
   show ApproachRight = "ApproachRight"
   show FollowMaxBeam = "FollowMaxBeam"
   show (Test1 _)     = "Test1 <...>"

instance Show Prim2 where
   show (Accel x)  = "Accel " ++ show x
   show (Brake x)  = "Brake " ++ show x
   show (Clutch x) = "Clutch " ++ show x
   show (Gear x)   = "Gear " ++ show x
   show (Steer x)  = "Steer " ++ show x
   show (Focus x)  = "Focus " ++ show x
   show (Meta x)   = "Meta " ++ show x
   show (Tick x)   = "Tick " ++ show x
   show (Test2 _)  = "Test2 <...>"

data SimpleDriver = SimpleDriver
type Sit1 = Sit Prim1
type Sit2 = Sit Prim2
type Conf1 = ConfIO Prim1 (Reward, Depth) IO
type Conf2 = ConfIO Prim2 (Reward, Depth) IO
type Prog1 = Prog Prim1
type Prog2 = Prog Prim2

instance BAT Prim1 where
   data Sit Prim1 = Sit1 {
      assoc :: Sit2
   }

   s0 = Sit1 s0

   do_ a@AntiDrift     s = s{assoc = sit $ refine a s (assoc s)}
   do_ a@AntiBlock     s = s{assoc = sit $ refine a s (assoc s)}
   do_ a@AntiSlip      s = s{assoc = sit $ refine a s (assoc s)}
   do_ a@ApproachLeft  s = s{assoc = sit $ refine a s (assoc s)}
   do_ a@DriveCenter   s = s{assoc = sit $ refine a s (assoc s)}
   do_ a@ApproachRight s = s{assoc = sit $ refine a s (assoc s)}
   do_ a@FollowMaxBeam s = s{assoc = sit $ refine a s (assoc s)}
   do_ (Test1 _)       s = s

   poss AntiDrift     _ = False
   poss AntiBlock     _ = False
   poss AntiSlip      _ = False
   poss ApproachLeft  s = trackTime (currentState1 s) Zero < 5 &&
                          trackTime (currentState1 s) Neg5 >
                             trackTime (currentState1 s) Zero
   poss DriveCenter   s = True-- trackTime (currentState1 s) Zero > 5
   poss ApproachRight s = trackTime (currentState1 s) Zero < 5 &&
                          trackTime (currentState1 s) Pos5 >
                             trackTime (currentState1 s) Zero
   poss FollowMaxBeam s = trackTime (currentState1 s) Zero < 3
   poss (Test1 f)  s = f s

instance DTBAT Prim1 where
   reward AntiDrift     _ = 1000
   reward AntiBlock     _ = 500
   reward AntiSlip      _ = 100
   reward ApproachLeft  _ = 1
   reward DriveCenter   _ = 1
   reward ApproachRight _ = 1
   reward FollowMaxBeam _ = 10
   reward (Test1 _)     _ = 0

instance IOBAT Prim1 IO where
   syncA a s =
      do putStrLn $ show a
         c2 <- dooSync' (refine a s (assoc s))
         if isNothing c2
            then fail $ "syncA: refinement of " ++ show a ++ " failed"
            else return ()
         putStrLn $ show a ++ " done"
         return $ (do_ a s){assoc = sit (fromJust c2)}

currentState1 :: Sit1 -> CarState
currentState1 = currentState2 . assoc

refine :: Prim1 -> Sit1 -> Sit2 -> Conf2
refine AntiDrift     _ s2 = refineProg Nil s2
refine AntiBlock     _ s2 = refineProg Nil s2
refine AntiSlip      _ s2 = refineProg Nil s2
refine ApproachLeft  _ s2 = refineProg (steerTrackPos    0.8) s2
refine DriveCenter   _ s2 = refineProg (steerTrackPos      0) s2
refine ApproachRight _ s2 = refineProg (steerTrackPos (-0.8)) s2
refine FollowMaxBeam _ s2 = refineProg (steerAngle longestBeam) s2
   where longestBeam = toRad $ snd $ maximum $
                       map (\ori -> (trackTime (currentState2 s2) ori, ori))
                       [Neg90 .. Pos90]
refine (Test1 _)     _ s2 = error "Test1 is not refinable"

refineProg :: Prog2 -> Sit2 -> Conf2
refineProg p1 s = treeDTIO 3 p s
   where p2 = (transmission `Seq` acceleration) `Seq` tick
         p  = p1 `Conc` star p2

instance BAT Prim2 where
   data Sit Prim2 = Sit2 {
      currentState2   :: CarState,
      currentControl2 :: CarControl,
      sensedState     :: IORef CarState,
      newControl      :: IORef CarControl,
      ticks           :: Sem.SSem,
      alreadyAccel    :: Bool,
      alreadyBrake    :: Bool,
      alreadyGear     :: Bool,
      alreadySteer    :: Bool
   }

   s0 = Sit2 defaultState defaultControl
             undefined undefined undefined
             False False False False

   do_ (Accel x)       s = modControl (\c -> c{accel         = x}) s{alreadyAccel = True}
   do_ (Brake x)       s = modControl (\c -> c{brake         = x}) s{alreadyBrake = True}
   do_ (Clutch x)      s = modControl (\c -> c{clutch        = x}) s
   do_ (Gear x)        s = modControl (\c -> c{Control.gear  = x}) s{alreadyGear  = True}
   do_ (Steer x)       s = modControl (\c -> c{steer         = x}) s{alreadySteer = True}
   do_ (Focus x)       s = modControl (\c -> c{Control.focus = x}) s
   do_ (Meta x)        s = modControl (\c -> c{meta          = x}) s
   do_ (Tick (Just x)) s = s{currentState2 = x,
                             alreadyAccel  = False,
                             alreadyBrake  = False,
                             alreadyGear   = False,
                             alreadySteer  = False}
   do_ (Tick Nothing)  s = modState (simulateState tickDurSec) s{alreadyAccel = False,
                                                                 alreadyBrake = False,
                                                                 alreadyGear  = False,
                                                                 alreadySteer = False}
   do_ (Test2 _)       s = s

   poss (Accel _)  s = not (alreadyAccel s) -- a < rpm (currentState2 s) / 7500
   poss (Brake _)  s = True
   poss (Clutch _) _ = True
   poss (Gear n)   s = True && abs (n - State.gear (currentState2 s)) <= 1
   poss (Steer _)  s = not (alreadySteer s) -- kmh2ms (speedX (currentState2 s)) > 1
   poss (Focus _)  _ = True
   poss (Meta _)   _ = True
   poss (Tick _)   _ = True
   poss (Test2 f)  s = f s

instance DTBAT Prim2 where
   reward (Accel _)  s = if alreadyAccel s then -1 else -0.01
   reward (Brake _)  s = if alreadyBrake s then -1 else -0.01
   reward (Clutch _) _ = 0
   reward (Gear _)   s = if alreadyGear  s then -1 else -0.01
   reward (Steer _)  s = if alreadySteer s then -1 else -0.01
   reward (Focus _)  _ = 0
   reward (Meta _)   _ = 0
   reward (Tick _)   s = if alreadyAccel s && alreadyBrake s &&
                            alreadyGear s && alreadySteer s then 0 else -1
   reward (Test2 _)  _ = 0

instance IOBAT Prim2 IO where
   syncA (Tick Nothing) s =
      do putStrLn $ "Current Control: " ++ show (currentControl2 s)
         putStrLn $ "Tick"
         Sem.wait (ticks s)
         sensed <- readIORef (sensedState s)
         let s' = do_ (Tick (Just sensed)) s
         putStrLn $ "Tack"
         return s'
   syncA (Tick (Just _)) _ =
         error "syncA: Tick (Just _) already synced"
   syncA a s =
      do let s' = do_ a s
         writeIORef (newControl s) (currentControl2 s)
         --putStrLn $ "Wrote " ++ show (currentControl2 s)
         return s'

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

transmission :: Prog2
transmission = primf (\s -> Gear (case lookup (g s) rpms of
                                       Just (lo,hi) | rpm' s < lo -> d s
                                                    | rpm' s > hi -> u s
                                       _                          -> g s))
   where rpm' = State.rpm . currentState2
         g    = State.gear . currentState2
         u    = (1+) . g
         d    = (1+) . g
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
               primf (\s -> if brake (currentControl2 s) > 0 then Accel 0 else Brake 0)
   where action state = let x = param state
                        in if x < 0 then Brake (-x) else Accel x
         param  state = if kmh2ms (speedX state) < 10 then 0.5 else param' state
         param' state = max (-1) $ min 1 $ (1 - 1 / (beamT oris)^(2::Int))
            where msX  = kmh2ms (speedX state)
                  msY  = kmh2ms (speedY state)
                  beta = atan2 msY msX
                  oris = find (\(lo,hi) -> toRad lo <= beta && beta <= toRad hi)
                         [(lo, succ lo) | lo <- [Neg90 .. pred Pos90]]
                  beamT (Just (lo,hi)) =
                         let r = (beta - toRad lo) / (toRad hi - toRad lo)
                         in r * trackTime state lo + (1-r) * trackTime state hi
                  beamT Nothing = 1/0

steerAngle :: Double -> Prog2
steerAngle ori = star action `Seq` success
   where action = primf (steerAngleAction ori)
         success = test (\s -> abs (angle (currentState2 s) - ori) < deg2rad 3)

steerAngleAction :: Double -> Sit2 -> Prim2
steerAngleAction ori s = Steer ori''
   where state = currentState2 s
         ori'' = if v > steerSensitivityOffset
                 then ori' / (steerLock * (v - steerSensitivityOffset))
                 else ori'
         ori' = ori * (angle state) * 0.5
         v    = kmh2ms (speedX state)
         steerLock = 0.366519 * 3.6
         steerSensitivityOffset = kmh2ms 80

steerTrackPos :: Double -> Prog2
steerTrackPos pos = star action `Seq` success
   where action     = primf (\s -> steerAngleAction (ori (currentState2 s)) s)
         ori state  = maxAngle * (diff state) / 2
         diff state = pos - trackPos state
         maxAngle   = deg2rad 45
         success    = test (\s -> abs (trackPos (currentState2 s) - pos) < 0.05)

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

data Angle =
   Neg90 | Neg75 | Neg60 | Neg45 | Neg30 | Neg20 | Neg15 | Neg10 | Neg5 | Zero |
   Pos5 | Pos10 | Pos15 | Pos20 | Pos30 | Pos45 | Pos60 | Pos75 | Pos90
   deriving (Show, Enum, Eq, Ord)

toRad :: Angle -> Double
toRad a = beamOris SimpleDriver !! fromEnum a

trackTime :: CarState -> Angle -> Double
trackTime state a = trackTime' state !! fromEnum a

trackDist :: CarState -> Angle -> Double
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
trackTime' :: CarState -> [Double]
trackTime' state = beamTimes
   where msX          = kmh2ms (speedX state)
         msY          = kmh2ms (speedY state)
         beta         = atan2 msY msX
         v            = sqrt (msX^(2::Int) + msY^(2::Int))
         beamOriDists = zip (beamOris SimpleDriver) (track' state)
         beamTimes    = map (\(alpha,d) -> d / (v * cos (beta - alpha))) beamOriDists

simulateState :: Double -> CarState -> CarState
simulateState t s = s{curLapTime    = curLapTime s + t,
                      distFromStart = distFromStart s + ms2kmh' distT,
                      distRaced     = distRaced s + ms2kmh' distT,
                      track         = map trackT (zip (beamOris SimpleDriver) (track s)),
                      trackPos      = trackPos s + trackPosT }
   where msX               = kmh2ms (speedX s)
         msY               = kmh2ms (speedY s)
         beta              = atan2 msY msX
         v                 = sqrt (msX^(2::Int) + msY^(2::Int))
         gamma             = angle s
         distT             = v * (cos (beta + gamma) - sin (beta + gamma)) * t
         trackT (alpha, d) = d + v * cos (beta - alpha) * t
         trackPosT         = v * sin (beta + gamma) * t / trackWidth
         trackWidth        = trackDist s Neg90 + trackDist s Pos90
         ms2kmh' x         = case ms2kmh x of KmH x' -> x'

instance Driver SimpleDriver where
   data Context SimpleDriver = Context (IORef CarState) (IORef CarControl)
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
--         _ <- printf "%.2f s  (%.2f m)    %.2f s  (%.2f m)    %.2f s  (%.2f m)\n"
--              (trackTime state Neg90) (trackDist state Neg90)
--              (trackTime state Zero)  (trackDist state Zero)
--              (trackTime state Pos90) (trackDist state Pos90)
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

