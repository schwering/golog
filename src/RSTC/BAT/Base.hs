{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Common base of BAT implementations using regression and progression.

module RSTC.BAT.Base (Prim(..), NTGCat(..), TTCCat(..), State(..),
                      lookahead, ntgDiff, ttcDiff, quality, match,
                      ntgCats, ttcCats, nan) where

import RSTC.Car
import Interpreter.Golog
import qualified RSTC.Obs as O
import RSTC.Theorems

data Prim a = Wait (Time a)
            | Accel Car (Accel a)
            | LaneChange Car Lane
            | forall b. O.Obs a b => Init b
            | forall b. O.Obs a b => Prematch b
            | forall b. O.Obs a b => Match b
            | Abort
            | NoOp
            | Start Car String
            | End Car String
            | Msg String

data NTGCat = VeryFarBehind
            | FarBehind
            | Behind
            | CloseBehind
            | VeryCloseBehind
            | SideBySide
            | VeryCloseInfront
            | CloseInfront
            | Infront
            | FarInfront
            | VeryFarInfront
            deriving (Bounded, Eq, Enum, Ord, Show)

data TTCCat = ConvergingFast
            | Converging
            | ConvergingSlowly
            | Reached
            | Stable
            | DivergingSlowly
            | Diverging
            | DivergingFast
            deriving (Bounded, Eq, Enum, Ord, Show)


class RealFloat a => State a b | b -> a where
   time :: b -> Time a
   lane :: b -> Car -> Lane
   ntg  :: b -> Car -> Car -> NTG a
   ttc  :: b -> Car -> Car -> TTC a


lookahead :: Depth
lookahead = 5


ntgDiff :: (State a b, O.Obs a c) => b -> c -> Car -> Car -> a
ntgDiff s e b c = ntg s b c - O.ntg e b c


ttcDiff :: (State a b, O.Obs a c) => b -> c -> Car -> Car -> a
ttcDiff s e b c = ttc s b c - O.ttc e b c


quality :: (State a b, O.Obs a c) => b -> c -> Car -> Car -> a
quality = ntgDiff


match :: (State a b, O.Obs a c) => c -> b -> Bool
match e s = let ntg_ttc = [(b, c, ntg s b c, O.ntg e b c,
                                  ttc s b c, O.ttc e b c) | b <- cars, c <- cars]
                ntgs  = [(ntg1, ntg2) | (b, c, ntg1, ntg2, _, _) <- ntg_ttc, b /= c]
                ttcs  = [(ttc1, ttc2, relVeloc' ntg1 ttc1, relVeloc' ntg2 ttc2) | (b, c, ntg1, ntg2, ttc1, ttc2) <- ntg_ttc, b < c]
                lanes = [(lane s b, O.lane e b) | b <- cars]
            in all (\(l1, l2) -> l1 == l2) lanes &&
               all (\(ntg1, ntg2) -> haveCommon (ntgCats ntg1)
                                                (ntgCats ntg2)) ntgs &&
               --XXX
               --all (\(ttc1, rv1, ttc2, rv2) -> haveCommon (ttcCats ttc1 rv1)
               --                                           (ttcCats ttc2 rv2)) ttcs &&
               True
   where haveCommon (x:xs) (y:ys) | x < y     = haveCommon xs (y:ys)
                                  | y < x     = haveCommon (x:xs) ys
                                  | otherwise = True
         haveCommon []     _                  = False
         haveCommon _      []                 = False


-- | Lists the NTG categories of a temporal distance.
ntgCats :: RealFloat a => NTG a -> [NTGCat]
ntgCats t = [cat | cat <- [minBound .. maxBound], inCat cat ]
   where inCat VeryFarBehind    = 5 <= t
         inCat FarBehind        = 3 <= t && t <= 7
         inCat Behind           = 2 <= t && t <= 4
         inCat CloseBehind      = 1 <= t && t <= 2.5
         inCat VeryCloseBehind  = 0 <= t && t <= 1.5
         inCat SideBySide       = -0.75 <= t && t <= 0.75
         inCat VeryCloseInfront = -1.5 <= t && t <= 0
         inCat CloseInfront     = -2.5 <= t && t <= -1
         inCat Infront          = -4 <= t && t <= -2
         inCat FarInfront       = -7 <= t && t <= -3
         inCat VeryFarInfront   = t <= -5


-- | Lists the TTC categories of a temporal distance.
--
-- There's a special case that deals with the real world's problems:
-- When two cars drive at the same speed, TTC is not defined in our model.
-- In reality, however, they probably never drive at the very same speed, but
-- instead balance their velocities so that they don't approach each other.
-- We simply hard-code this case by saying the time to collision must be at
-- least 30.
ttcCats :: RealFloat a => TTC a -> a -> [TTCCat]
ttcCats t rv = [cat | cat <- [minBound .. maxBound], inCat cat ]
   where inCat ConvergingSlowly = 10 <= t
         inCat Converging       = 3.5 <= t && t <= 12
         inCat ConvergingFast   = 0 <= t && t <= 5
         inCat Reached          = -2 <= t && t <= 2
         inCat DivergingFast    = -5 <= t && t <= 0
         inCat Diverging        = -12 <= t && t <= -3.5
         inCat DivergingSlowly  = t <= -10
         inCat Stable           = isNaN t ||
                                  abs (1 - rv) <= 0.03


nan :: RealFloat a => a
nan = (0 /) $! 0

