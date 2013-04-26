{-# LANGUAGE FlexibleContexts #-}

-- | Golog programs based on the RSTC action theory.

module RSTC.Progs where

import RSTC.Car
import Interpreter.Golog
import RSTC.BAT.Progression
import qualified RSTC.Obs as Obs
import RSTC.Theorems
import Util.NativePSO

import Data.Maybe
import Debug.Trace

picknum :: (Double, Double) -> (Double -> Double) -> Double
picknum bounds val = pso 10 m n defaultParams bounds (Max val)
   where m = 10
         n = 1


act :: a -> Prog a
act = PseudoAtom . Atom . Prim


actf :: (Sit a -> a) -> Prog a
actf = PseudoAtom . Atom . PrimF


test :: (Sit a -> Bool) -> Prog a
test = PseudoAtom . Atom . Test


ptest :: String -> Prog a
ptest s = test (const (traceStack s True))


atomic :: Prog a -> Prog a
atomic = PseudoAtom . Complex


obsprog :: (Obs.Obs Double b) => [Maybe b] -> Prog (Prim Double)
obsprog []     = Nil
obsprog (e:es) = seq' (initAct:acts)
   where initAct = maybe Nil (act . Init) e
         acts = map (\e' -> atomic ((actf (\s -> Wait (Obs.time e' - time s)))
                              --`Seq` (act (Prematch e'))
                              `Seq` (act (Match e'))))
                    (catMaybes es)
         seq' []     = Nil
         seq' (p:ps) = Seq p (seq' ps)


follow :: Car -> Car -> Prog (Prim Double)
follow b c =
   atomic (
      act (Start b "follow") `Seq`
      test (\s -> lane s b == lane s c) `Seq`
      test (\s -> isFollowing (ntg s) b c) `Seq`
      test (\s -> (CloseBehind `elem` (ntgCats (ntg s b c)))) `Seq`
      actf (\s -> Accel b (relVeloc (ntg s) (ttc s) c b))
   ) `Seq`
   --Pick (\val -> picknum (0, 2) (fst.val)) 1 (\q -> act (Accel b q)) `Seq`
   act (End b "follow")


tailgate :: Car -> Car -> Prog (Prim Double)
tailgate b c =
   atomic (
      act (Start b "tailgate") `Seq`
      test (\s -> lane s b == lane s c) `Seq`
      test (\s -> isFollowing (ntg s) b c) `Seq`
      test (\s -> any (`elem` (ntgCats (ntg s b c))) [VeryCloseBehind, CloseBehind]) `Seq`
      actf (\s -> Accel b (relVeloc (ntg s) (ttc s) c b))
   ) `Seq`
   --Pick (\val -> picknum (0, 2) (fst.val)) 1 (\q -> act (Accel b q)) `Seq`
   act (End b "tailgate")


{-
pass :: Car -> Car -> Prog (Prim Double)
pass b c =
   atomic (
      act (Start b "pass") `Seq`
      test (\s -> lane s b /= lane s c) `Seq`
      test (\s -> isFollowing (ntg s) b c) `Seq`
      test (\s -> isConverging (ttc s) b c)
   ) `Seq` (
      Star (Pick (fst . value lookahead) (picknum (0.95, 1.2)) (\q -> act (Accel b q)))
   ) `Seq` atomic (
      test (\s -> ntg s b c <= 0) `Seq`
      act (End b "pass") 
   )
-}


pass :: Car -> Car -> Prog (Prim Double)
pass b c =
   atomic (
      act (Start b "pass") `Seq`
      test (\s -> lane s b /= lane s c) `Seq`
      test (\s -> isFollowing (ntg s) b c)
   ) `Seq` (
      Star (actf (\s -> Accel b (bestAccel s b c)))
   ) `Seq` atomic (
      test (\s -> ntg s b c < 0) `Seq`
      act (End b "pass") 
   )


overtake :: Car -> Car -> Prog (Prim Double)
overtake b c =
   atomic (
      act (Start b "overtake") `Seq`
      test (\s -> lane s b == lane s c) `Seq`
      test (\s -> isFollowing (ntg s) b c)
      --test (\s -> isConverging (ttc s) b c)
   ) `Seq` (
      (
         act (LaneChange b LeftLane) `Seq`
         test (\s -> ntg s b c < 0) `Seq`
         act (LaneChange b RightLane)
      ) `Conc` (
         --Star (Pick (fst . value lookahead) (picknum (0.9, 1.5)) (\q -> act (Accel b q)))
         {-
         Star (Pick (valueByQuality b c lookahead)
                    --(\val -> interpolateRecipLin id (0.7, 1.5) 0 (fromMaybe 100 . val))
                    --(\val -> interpolateRecipLinAndLinForZero id (0.7, 1.5) (fromMaybe (100, 100) . val))
                    (\val -> 0.5 * nullAt id (canonicalizeRecip (fst . fromMaybe (nan,nan) . val) 0) +
                             0.5 * nullAt id (canonicalize      (snd . fromMaybe (nan,nan) . val) 0))
                    (\q -> act (Accel b q)))
                    --(\q -> (act (Accel b (q)) `Seq` (actf (\s -> Msg (show (sitlen s)))))))
         -}
         --Star (actf (\s -> Accel b (bestAccel s b c)))
         Star (actf (\s -> Accel b (bestAccel s b c)))
      )
   ) `Seq` atomic (
      test (\s -> ntg s b c < 0) `Seq`
      act (End b "overtake") 
   )

