{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module TORCS.Golog.BAT.WarmUp where

import Control.Concurrent.MSampleVar
import Control.Monad ((>=>))
import Data.IORef
import Data.Maybe (fromMaybe)
import Golog.Interpreter
import Golog.Macro hiding (loop)
import qualified Golog.Macro as GM
import Golog.Util
import TORCS.CarControl
import TORCS.CarState
import qualified TORCS.Golog.BAT.GearBox as G
import qualified TORCS.Golog.BAT.Perception as P
import qualified TORCS.Golog.BAT.Traction as T
import TORCS.Golog.Sensors
import TORCS.PhysicsUtil

data A = Drive | Tick

type ConfIO a = Conf a (Sync a IO)

cnf :: IOBAT a IO => Sit a -> ConfIO a
cnf = treeNDIO Nil

instance BAT A where
   data Sit A = Sit {
      gc :: ConfIO G.A,
      pc :: ConfIO P.A,
      tc :: ConfIO T.A
   }

   s0 = Sit (cnf s0) (cnf s0) (cnf s0)

   do_ a@Drive s = s{gc = refineG a s (sit (gc s)),
                     tc = refineT a s (sit (tc s))}
   do_ a@Tick  s = s{pc = refineP a s (sit (pc s))}

   poss _ _ = True

mkS0 :: MSampleVar CarState -> IORef CarControl -> Sit A
mkS0 csVar ccRef = s0{gc = cnf (G.mkS0 ccRef),
                      pc = cnf (P.mkS0 csVar),
                      tc = cnf (T.mkS0 ccRef)}

cc :: Sit A -> CarControl
cc s = G.fillCc (sit (gc s)) $ T.fillCc (sit (tc s)) $ defaultControl

cs :: Sit A -> CarState
cs s = P.cs (sit (pc s))

type RefineF a = A -> Sit A -> Sit a -> ConfIO a

refine :: IOBAT a IO => String -> Sit a -> Prog a -> ConfIO a
refine name s p = case doo' (treeNDIO p s) of
                       Just c  -> c
                       Nothing -> error $ name ++ ": execution failed"

refineG :: RefineF G.A
refineG Drive s s' = refine "refineG" s' $ G.sense (cs s) `Seq` G.transmission
refineG Tick  _ _  = error "refineG: unrefinable action"

refineP :: RefineF P.A
refineP Tick  s s' = refine "refineP" s' $ P.sense (cc s)
refineP Drive _ _  = error "refineP: unrefinable action"

refineT :: RefineF T.A
refineT Drive s s' = refine "refineT" s' $ T.sense (cs s) `Seq` keepCentered s
refineT Tick  _ _  = error "refineT: unrefinable action"

instance IOBAT A IO where
   syncA a@Drive s =
      do let s' = do_ a s
         gc' <- sync (gc s')
         tc' <- sync (tc s')
         return s'{gc = gc', tc = tc'}
   syncA a@Tick s =
      do let s' = do_ a s
         pc' <- sync (pc s')
         return s'{pc = pc'}

keepCentered :: Sit A -> Prog T.A
keepCentered s = atomic $ if_ tooFast (then_ slowDown) (else_ speedUp) `Seq` steer
   where tooFast _ | speedX (cs s) < kmh2ms 50  = False
                   | brakeD >= beamD            = True
                   | speedX (cs s) > kmh2ms 225 = True
                   | otherwise                  = False
            where -- Current velocity in m/s.
                  (theta, v0) = speed (cs s)
                  -- Target velocity in m/s.
                  v1 = kmh2ms 50
                  -- Maximum deceleration in m/(s*s)
                  b = 10
                  -- Braking distance.
                  -- The velocity after braking for time period t is
                  --   v(t) = v0 - b*t
                  -- and thus the integral of this is the driven distance
                  --   s(t) = v0*t - 1/2*b*t^2.
                  -- The target velocity v1 is reached after time t1
                  --   v(t1) = v1  <=>  t1 = (v0 - v1) / b
                  -- and thus we get the braking distance
                  --   s(t1) = 1/b * (1/2*v0^2 - v1^2).
                  brakeD = 1 / b * (1/2*v0*v0 - v1*v1)
                  -- The laser beam in car direction tells us the distance we
                  -- can drive safely.
                  beamD = fromMaybe (trackDist (cs s) Zero)
                                    (projectBeam (trackDist (cs s)) theta)
         slowDown = T.brake 1 `Seq` T.accel 0
         speedUp  = T.accel 1 `Seq` T.brake 0
         steer    = T.steer (angle (cs s) - trackPos (cs s) / 2)

gologAgent :: MSampleVar CarState -> IORef CarControl -> MSampleVar (Sit A) -> IO ()
gologAgent csVar ccRef sitVar = loop conf
   where conf = treeNDIO (GM.loop (prim Tick `Seq` prim Drive)) (mkS0 csVar ccRef)
         loop c = maybe (putStrLn "EOP") (sync >=> store >=> loop) (trans' c)
         store c = writeSV sitVar (sit c) >> return c

