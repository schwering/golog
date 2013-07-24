{-# LANGUAGE TypeFamilies #-}

-- | A simple BAT for manual comparison between the old and new Golog
-- interpreters.
module GologTest where

import Golog.Interpreter
import qualified Golog.Macro as M
import Golog.Util

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

star = M.star
nondet = Nondet
conc = foldl1 Conc
atomic = M.atomic

doDT :: DTBAT a => Depth -> Prog a -> Sit a -> [Sit a]
doDT l p s = map sit $ doo' (treeDT l p s)

rewardSum :: Sit Int -> Reward
rewardSum S0       = 0
rewardSum (Do a s) = rewardSum s + reward a s

