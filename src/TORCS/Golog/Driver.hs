{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}

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
module TORCS.Golog.Driver (gologDriver) where

import Control.Concurrent
import qualified Control.Concurrent.SSem as Sem
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import System.Timeout (timeout)
import Text.Printf
import TORCS.CarControl
import TORCS.CarState
import TORCS.Client
import TORCS.Golog.BAT.WarmUp
import TORCS.Golog.Sensors hiding (beamOris)
import qualified TORCS.Golog.Sensors as Sensors
import TORCS.Golog.Visualization
import TORCS.PhysicsUtil

data GD

gologDriver :: GD
gologDriver = undefined

instance Driver GD where
   data Context GD = Context (IORef CarState) (IORef CarControl)
                              Sem.SSem ThreadId ThreadId UTCTime

   initialState _ = do  csRef <- newIORef defaultState
                        ccRef <- newIORef defaultControl
                        tickSem <- Sem.new 0
                        gologThread <- forkOS $ gologAgent csRef ccRef tickSem
                        visThread <- forkOS $ visualize csRef ccRef
                        startTime <- getCurrentTime
                        return (Context csRef ccRef tickSem gologThread visThread startTime)

   command ctx@(Context csRef ccRef tickSem _ _ startTime) cs' =
      do writeIORef csRef cs'
         --putStrLn $ show cs'
         _ <- Sem.signal tickSem
         --now <- getCurrentTime
         --let diff = (curLapTime cs') - realToFrac (diffUTCTime now startTime)
         --printf (kred ++ "time-diff = %.4f" ++ knrm ++ "\n") diff
         --printf (kred ++ "gear = %d" ++ knrm ++ "\n") (gear cs')
         --_ <- printf (kred ++
         --             "time = %.2f  " ++
         --             "pos = %.2f  " ++
         --             "angle = %.2f  " ++
         --             "vX = %.2f  " ++
         --             "vY = %.2f  " ++
         --             "track[-5] = %.2f s = %.2f m " ++
         --             "track[0] = %.2f s = %.2f m " ++
         --             "track[5] = %.2f s = %.2f m " ++
         --             knrm ++ "\n")
         --      (curLapTime cs')
         --      (trackPos cs')
         --      (deg $ rad2deg $ angle cs')
         --      (speedX cs')
         --      (speedY cs')
         --      (trackTime cs' Neg5) (trackDist cs' Neg5)
         --      (trackTime cs' Zero)  (trackDist cs' Zero)
         --      (trackTime cs' Pos5) (trackDist cs' Pos5)
         --putStrLn (kmag ++ show (track cs') ++ knrm)
         --cc <- timeout (tickDurMicroSec) $ readIORef ccRef
         threadDelay (tickDurMicroSec * 8 `div` 10)
         cc' <- readIORef ccRef
         --printf (kblu ++ "gear = %d" ++ knrm ++ "\n") (gearCmd cc')
         --printf (kblu ++
         --        "accel = %.2f  " ++
         --        "brake = %.2f  " ++
         --        "gear = %d  " ++
         --        "steer = %.2f  " ++
         --        knrm ++ "\n")
         --       (accelCmd cc')
         --       (brakeCmd cc')
         --       (gearCmd cc')
         --       (steerCmd cc')
         return (ctx, cc')

   shutdown ctx =
      do putStrLn "SHUTDOWN"
         --killThread gologThread
         ctx' <- restart ctx
         return (Just ctx')

   restart (Context csRef ccRef _ gologThread visThread _) =
      do putStrLn "RESTART"
         killThread gologThread
         tickSem <- Sem.new 0
         writeIORef csRef defaultState
         writeIORef ccRef defaultControl
         newGologThread <- forkOS $ gologAgent csRef ccRef tickSem
         startTime <- getCurrentTime
         return (Context csRef ccRef tickSem newGologThread visThread startTime)

   beamOris _ = Sensors.beamOris

tickDurMicroSec :: Int
tickDurMicroSec = 10 * 1000

