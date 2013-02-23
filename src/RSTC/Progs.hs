{-# LANGUAGE MultiParamTypeClasses #-}

module RSTC.Progs where

import Car
import Interpreter.Golog
import RSTC.BAT
import qualified RSTC.BAT as BAT
import qualified RSTC.Obs as Obs
import RSTC.Theorems
import Util.NativePSO

import Data.Maybe

interpol :: (Fractional a, Ord a) => (a -> a) -> a -> a -> a -> Maybe a
interpol f lo hi goal
   | f lo <= goal && goal <= f hi  = let m = (goal - (f lo)) / ((f hi) - (f lo))
                                     in Just (lo + m * (hi - lo))
   | f lo > f hi                   = interpol (\x -> -(f x)) lo hi (-goal)
   | otherwise                     = Nothing


picknum :: (Double, Double) -> (Double -> (Reward, Depth)) -> Double
picknum bounds val = (pso 10 m n defaultParams bounds (Max (fst . val)))
   where m = 20
         n = 1


act :: a -> Prog a
act a = PseudoAtom (Atom (Prim a))


actf :: (Sit a -> a) -> Prog a
actf a = PseudoAtom (Atom (PrimF a))


test :: (Sit a -> Bool) -> Prog a
test t = PseudoAtom (Atom (Test t))


atomic :: Prog a -> Prog a
atomic p = PseudoAtom (Complex p)


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
      Pick (picknum (0, 2)) 1 (\q -> act (Accel b q)) `Seq`
      act (End b "follow")
   )


pass :: Car -> Car -> Prog (Prim Double)
pass b c =
   atomic (
      act (Start b "pass") `Seq`
      test (\s -> lane b s == lane c s) `Seq`
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
      test (\s -> isFollowing (BAT.ntg s) b c) `Seq`
      test (\s -> isConverging (BAT.ttc s) b c)
   ) `Seq` (
      (
         act (LaneChange b LeftLane) `Seq`
         test (\s -> BAT.ntg s b c < 0) `Seq`
         act (LaneChange b RightLane)
      ) `Conc` (
         Star (Pick (picknum (0.95, 1.2)) 1 (\q -> act (Accel b q)))
      )
   ) `Seq` atomic (
      test (\s -> BAT.ntg s b c < 0) `Seq`
      act (End b "overtake") 
   )

