{-# LANGUAGE TypeFamilies #-}

-- | A simple BAT for manual comparison between the old and new Golog
-- interpreters.
module OldGologTest where

import Golog.Old.Interpreter

instance BAT Int where
   data Sit Int = S0 | Do Int (Sit Int) deriving Show
   s0 = S0
   do_ = Do
   poss a _ = even a
   reward _ s         | sitlen s > 5 = -1000
   reward a (Do a' _) | a == a'      = -1
   reward a _                        = fromIntegral (max 0 a)

sitlen :: Sit Int -> Int
sitlen S0       = 0
sitlen (Do _ s) = 1 + sitlen s

p = PseudoAtom . Atom . Prim
q = PseudoAtom . Complex

iter p = Nil `Nondet` (p `Seq` iter p)
--iter p = Nondet Nil (plus p)
--iter = Star
plus p = Nondet p (p `Seq` plus p)
choice = foldl1 Nondet
conc = foldl1 Conc
atomic = PseudoAtom . Complex

doDT :: BAT a => Depth -> Prog a -> Sit a -> [Sit a]
doDT l p s = map (\(s,_,_) -> s) $ do3 l p s

