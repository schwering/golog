{-# LANGUAGE FlexibleContexts #-}

-- | Golog programs based on the RSTC action theory.

module RSTC.Progs where

import RSTC.Car
import Interpreter.Golog2
import Interpreter.Golog2Util
import RSTC.BAT.Progression
import qualified RSTC.Obs as O
import RSTC.Theorems
import Util.NativePSO

import Data.Maybe
import Debug.Trace

--star = Star
--atomic = PseudoAtom . Complex
--prim = PseudoAtom . Atom . Prim
--primf = PseudoAtom . Atom . PrimF
--test = PseudoAtom . Atom . Test

picknum :: (Double, Double) -> (Double -> Double) -> Double
picknum bounds val = pso 10 m n defaultParams bounds (Max val)
   where m = 10
         n = 1


ptest :: String -> Prog a
ptest s = test (const (traceStack s True))


obsprog :: State a => [Maybe O.ObsId] -> Prog (Prim a)
obsprog []     = Nil
obsprog (e:es) = seq' (initAct:acts)
   where initAct = maybe Nil (prim . Init) e
         acts = map (\e' -> atomic ((primf (\s -> Wait (O.time e' - time s)))
                              --`Seq` (prim (Prematch e'))
                              `Seq` (prim (Match e'))))
                    (catMaybes es)
         seq' []     = Nil
         seq' (p:ps) = Seq p (seq' ps)


follow :: State a => Car -> Car -> Prog (Prim a)
follow b c =
   atomic (
      prim (Start b "follow") `Seq`
      test (\s -> lane s b == lane s c) `Seq`
      test (\s -> isFollowing (ntg s) b c) `Seq`
      test (\s -> (CloseBehind `elem` (ntgCats (ntg s b c)))) `Seq`
      primf (\s -> Accel b (relVeloc (ntg s) (ttc s) c b))
   ) `Seq`
   --Pick (\val -> picknum (0, 2) (fst.val)) 1 (\q -> prim (Accel b q)) `Seq`
   prim (End b "follow")


tailgate :: State a => Car -> Car -> Prog (Prim a)
tailgate b c =
   atomic (
      prim (Start b "tailgate") `Seq`
      test (\s -> lane s b == lane s c) `Seq`
      test (\s -> isFollowing (ntg s) b c) `Seq`
      test (\s -> any (`elem` (ntgCats (ntg s b c))) [VeryCloseBehind, CloseBehind]) `Seq`
      primf (\s -> Accel b (relVeloc (ntg s) (ttc s) c b))
   ) `Seq`
   --Pick (\val -> picknum (0, 2) (fst.val)) 1 (\q -> prim (Accel b q)) `Seq`
   prim (End b "tailgate")


{-
pass :: Car -> Car -> Prog (Prim Double)
pass b c =
   atomic (
      prim (Start b "pass") `Seq`
      test (\s -> lane s b /= lane s c) `Seq`
      test (\s -> isFollowing (ntg s) b c) `Seq`
      test (\s -> isConverging (ttc s) b c)
   ) `Seq` (
      star (Pick (fst . value lookahead) (picknum (0.95, 1.2)) (\q -> prim (Accel b q)))
   ) `Seq` atomic (
      test (\s -> ntg s b c <= 0) `Seq`
      prim (End b "pass") 
   )
-}


pass :: HistState a => Car -> Car -> Prog (Prim a)
pass b c =
   atomic (
      prim (Start b "pass") `Seq`
      test (\s -> lane s b /= lane s c) `Seq`
      test (\s -> isFollowing (ntg s) b c)
   ) `Seq` (
      star (primf (\s -> Accel b (bestAccel s b c)))
   ) `Seq` atomic (
      test (\s -> ntg s b c < 0) `Seq`
      prim (End b "pass") 
   )


overtake :: HistState a => Car -> Car -> Prog (Prim a)
overtake b c =
   atomic (
      prim (Start b "overtake") `Seq`
      test (\s -> lane s b == lane s c) `Seq`
      test (\s -> isFollowing (ntg s) b c)
      --test (\s -> isConverging (ttc s) b c)
   ) `Seq` (
      (
         prim (LaneChange b LeftLane) `Seq`
         test (\s -> ntg s b c < 0) `Seq`
         prim (LaneChange b RightLane)
      ) `Conc` (
         --star (Pick (fst . value lookahead) (picknum (0.9, 1.5)) (\q -> prim (Accel b q)))
         {-
         star (Pick (valueByQuality b c lookahead)
                    --(\val -> interpolateRecipLin id (0.7, 1.5) 0 (fromMaybe 100 . val))
                    --(\val -> interpolateRecipLinAndLinForZero id (0.7, 1.5) (fromMaybe (100, 100) . val))
                    (\val -> 0.5 * nullAt id (canonicalizeRecip (fst . fromMaybe (nan,nan) . val) 0) +
                             0.5 * nullAt id (canonicalize      (snd . fromMaybe (nan,nan) . val) 0))
                    (\q -> prim (Accel b q)))
                    --(\q -> (prim (Accel b (q)) `Seq` (primf (\s -> Msg (show (sitlen s)))))))
         -}
         --star (primf (\s -> Accel b (bestAccel s b c)))
         star (primf (\s -> Accel b (bestAccel s b c)))
      )
   ) `Seq` atomic (
      test (\s -> ntg s b c < 0) `Seq`
      prim (End b "overtake") 
   )

