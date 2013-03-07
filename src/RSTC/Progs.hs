-- | Golog programs based on the RSTC action theory.

{-# LANGUAGE MultiParamTypeClasses #-}

module RSTC.Progs where

import RSTC.Car
import Interpreter.Golog
import RSTC.BAT
import qualified RSTC.BAT as BAT
import qualified RSTC.Obs as Obs
import RSTC.Theorems
import Util.NativePSO

import Data.Maybe
import System.IO.Unsafe
import Unsafe.Coerce

interpol :: (Fractional a, Ord a) => (a -> a) -> a -> a -> a -> Maybe a
interpol f lo hi goal
   | f lo <= goal && goal <= f hi  = let m = (goal - (f lo)) / ((f hi) - (f lo))
                                     in Just (lo + m * (hi - lo))
   | f lo > f hi                   = interpol (\x -> -(f x)) lo hi (-goal)
   | otherwise                     = Nothing


--picknum :: (Double, Double) -> (Double -> (Reward, Depth)) -> Double
--picknum bounds val = pso 10 m n defaultParams bounds (Max (fst . val))
--XXX TODO val has now type (Double -> v) for some Ord v
picknum :: Ord v => (Double, Double) -> (Double -> v) -> Double
picknum bounds val = pso 10 m n defaultParams bounds (Max f)
   where f = \x -> fst (unsafeCoerce (val x))
         m = 10
         n = 1


act :: a -> Prog a
act = PseudoAtom . Atom . Prim


actf :: (Sit a -> a) -> Prog a
actf = PseudoAtom . Atom . PrimF


test :: (Sit a -> Bool) -> Prog a
test = PseudoAtom . Atom . Test


ptest :: String -> Prog a
ptest s = test (\_ -> unsafePerformIO (putStrLn s >>= \_ -> return True))


atomic :: Prog a -> Prog a
atomic = PseudoAtom . Complex


obsprog :: (Obs.Obs a b) => [Maybe b] -> Prog (Prim a)
obsprog []     = Nil
obsprog (e:es) = seq' (initAct:acts)
   where initAct = maybe Nil (act . Init) e
         acts = map (\e' -> atomic ((actf (\s -> Wait (Obs.time e' - start s)))
                              `Seq` (act (Match e'))))
                    (catMaybes es)
         seq' []     = Nil
         seq' (p:ps) = Seq p (seq' ps)


follow :: Car -> Car -> Prog (Prim Double)
follow b c =
   atomic (
      act (Start b "follow") `Seq`
      test (\s -> lane b s == lane c s) `Seq`
      test (\s -> isFollowing (BAT.ntg s) b c) `Seq`
      test (\s -> (CloseBehind `elem` (ntgCats (BAT.ntg s b c)))) `Seq`
      actf (\s -> Accel b (relVeloc (BAT.ntg s) (BAT.ttc s) c b))
   ) `Seq`
   --Pick (picknum (0, 2)) 1 (\q -> act (Accel b q)) `Seq`
   act (End b "follow")


tailgate :: Car -> Car -> Prog (Prim Double)
tailgate b c =
   atomic (
      act (Start b "tailgate") `Seq`
      test (\s -> lane b s == lane c s) `Seq`
      test (\s -> isFollowing (BAT.ntg s) b c) `Seq`
      test (\s -> any (`elem` (ntgCats (BAT.ntg s b c))) [VeryCloseBehind, CloseBehind]) `Seq`
      actf (\s -> Accel b (relVeloc (BAT.ntg s) (BAT.ttc s) c b))
   ) `Seq`
   --Pick (picknum (0, 2)) 1 (\q -> act (Accel b q)) `Seq`
   act (End b "tailgate")


pass :: Car -> Car -> Prog (Prim Double)
pass b c =
   atomic (
      act (Start b "pass") `Seq`
      test (\s -> lane b s /= lane c s) `Seq`
      test (\s -> isFollowing (BAT.ntg s) b c) `Seq`
      test (\s -> isConverging (BAT.ttc s) b c)
   ) `Seq` (
      Star (Pick (picknum (0.95, 1.2)) 1 (\q -> act (Accel b q)))
   ) `Seq` atomic (
      test (\s -> BAT.ntg s b c <= 0) `Seq`
      act (End b "pass") 
   )


overtake :: Car -> Car -> Prog (Prim Double)
overtake b c =
   atomic (
      act (Start b "overtake") `Seq`
      test (\s -> lane b s == lane c s) `Seq`
      ptest "huhu1" `Seq`
      test (\s -> isFollowing (BAT.ntg s) b c) `Seq`
      ptest "huhu2" `Seq`
      --test (\s -> isConverging (BAT.ttc s) b c) `Seq`
      ptest "huhu3"
   ) `Seq` (
      (
         act (LaneChange b LeftLane) `Seq`
         test (\s -> BAT.ntg s b c < 0) `Seq`
         act (LaneChange b RightLane)
      ) `Conc` (
         Star (Pick (picknum (0.9, 1.5)) 1 (\q -> act (Accel b q)))
      )
   ) `Seq` atomic (
      test (\s -> BAT.ntg s b c < 0) `Seq`
      act (End b "overtake") 
   )

