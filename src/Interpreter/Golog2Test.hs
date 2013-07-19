{-# LANGUAGE TypeFamilies #-}

module Interpreter.Golog2Test where

import Interpreter.Golog2
import qualified Interpreter.Golog2Util as U

instance BAT Int where
   data Sit Int = S0 | Do Int (Sit Int) deriving Show
   s0 = S0
   do_ = Do
   poss a _ = even a

instance DTBAT Int where
   reward _ s         | sitlen s > 5 = -1000
   reward a (Do a' _) | a == a'      = -1
   reward a _                        = fromIntegral (max 0 a)

sitlen :: Sit Int -> Int
sitlen S0       = 0
sitlen (Do _ s) = 1 + sitlen s

p = PseudoAtom . Atom . Prim
q = PseudoAtom . Complex

star = U.star
nondet = Nondet
conc = foldl1 Conc
atomic = U.atomic

doDT :: DTBAT a => Depth -> Prog a -> Sit a -> [Sit a]
doDT l p s = map sit $ do2 (treeDT l p s)

allReward :: Sit Int -> Reward
allReward S0       = 0
allReward (Do a s) = allReward s + reward a s

