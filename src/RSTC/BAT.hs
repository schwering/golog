-- | Basic action theory based on relative temporal measures.
--
-- The two measures are net time gap (NTG) and time to collision (TTC).
-- The successor state axioms (SSAs) use the 'RSTC.Theorems' module.

{-# LANGUAGE ExistentialQuantification #-}

module RSTC.BAT where

import RSTC.Car
import Interpreter.Golog
import qualified RSTC.Obs as O
import RSTC.Theorems
import Util.Memo

data Prim a = Wait (Time a)
            | Accel Car (Accel a)
            | LaneChange Car Lane
            | forall b. O.Obs a b => Init b
            | forall b. O.Obs a b => Match b
            | Abort
            | NoOp
            | Start Car String
            | End Car String

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

----------
-- Precondition and reward.

instance (RealFloat a, Show a) => BAT (Prim a) where
   poss (Wait t)             _ = t >= 0
   poss a @ (Accel b q)      s = noDupe a s
   poss a @ (LaneChange b l) s = l /= lane b s && noDupe a s
   poss (Init _)             _ = True
   poss (Match e)            s = match e s
   poss Abort                _ = False
   poss NoOp                 _ = True
   poss (Start _ _)          _ = True
   poss (End _ _)            _ = True

   reward (Wait _)         _                = 0
   reward (Accel _ _)      _                = -0.01
   reward (LaneChange _ _) _                = -0.01
   reward (Init _)         _                = 0
   reward (Match _)        _                = 1
   reward Abort            _                = 0
   reward NoOp             _                = 0
   reward (Start _ _)      s                = max 0 (1000 - 2 * (fromIntegral (sitlen s)))
   reward (End _ _)        (Do (Match _) s) = 2 * (fromIntegral (sitlen s))
   reward (End _ _)        _                = 0


sitlen :: Sit a -> Int
sitlen (Do _ s) = 1 + (sitlen s)
sitlen S0       = 0


match :: (RealFloat a, O.Obs a b, Show a) => b -> Sit (Prim a) -> Bool
match e s = let ntg_ttc = [(b, c, ntg s b c, O.ntg e b c,
                                  ttc s b c, O.ttc e b c) | b <- cars, c <- cars]
                ntgs  = [(ntg1, ntg2)
                           | (b, c, ntg1, ntg2, _, _) <- ntg_ttc, b /= c]
                ttcs  = [(ttc1, ttc2, relVeloc' ntg1 ttc1, relVeloc' ntg2 ttc2)
                           | (b, c, ntg1, ntg2, ttc1, ttc2) <- ntg_ttc, b < c]
                lanes = [(lane b s, O.lane e b) | b <- cars]
            in all (\(l1, l2) -> l1 == l2) lanes &&
               all (\(ntg1, ntg2) -> haveCommon (debug' ("NTG1 " ++ (show ntg1)) $ ntgCats ntg1)
                                                (debug' ("NTG2 " ++ (show ntg2)) $ ntgCats ntg2)) ntgs &&
               all (\(ttc1, rv1, ttc2, rv2) -> haveCommon (debug' ("TTC1 " ++ (show ttc1)) $ ttcCats ttc1 rv1)
                                                          (debug' ("TTC2 " ++ (show ttc2)) $ ttcCats ttc2 rv2)) ttcs
   where haveCommon (x:xs) (y:ys) | x < y     = haveCommon xs (y:ys)
                                  | y < x     = haveCommon (x:xs) ys
                                  | otherwise = True
         haveCommon []     _                  = False
         haveCommon _      []                 = False


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
ttcCats :: (RealFloat a, Show a) => TTC a -> a -> [TTCCat]
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


noDupe :: Prim a -> Sit (Prim a) -> Bool
noDupe a' @ (Accel b _) (Do a s) = case a
   of Wait _         -> True
      Accel c _      -> c < b
      _              -> noDupe a' s
noDupe a' @ (LaneChange b _) (Do a s) = case a
   of Wait _         -> True
      LaneChange c _ -> c < b
      _              -> noDupe a' s
noDupe _ S0 = True
noDupe _ _ = error "RSTC.BAT.noDupe: neither Accel nor LaneChange"


----------
-- Successor State Axioms.


start :: RealFloat a => Sit (Prim a) -> Time a
start (Do (Wait t) s) = t + (start s)
start (Do _ s)        = start s
start S0              = 0

-- | SSA for lane.
lane :: RealFloat a => Car -> Sit (Prim a) -> Lane
lane b (Do (LaneChange c l) _) | b == c = l
lane b (Do (Init e) _)                  = O.lane e b
lane b (Do _ s)                         = lane b s
lane _ S0                               = RightLane


-- | SSA of NTG. For Accel actions, transitivity is tried.
-- Situation argument is first for better currying inside the SSAs.
-- This is the memoizing function.
ntg :: RealFloat a => Sit (Prim a) -> Car -> Car -> NTG a
ntg = memo'' ntg'


-- | SSA of NTG. For Accel actions, transitivity is tried.
-- Situation argument is first for better currying inside the SSAs.
-- This is the non-memoized function doing the actual work.
ntg' :: RealFloat a => Sit (Prim a) -> Car -> Car -> NTG a
ntg' _                  b c | b == c = nan
ntg' (Do (Wait t) s)    b c          = orTrans
                                          (tntg (ntg s) (ttc s) t)
                                          (ntgTrans (tntg (ntg s) (ttc s) t)
                                                    (tttc (ntg s) (ttc s) t))
                                          b c
ntg' (Do (Accel d q) s) b c | b == d = antg1 (ntg s) (ttc s) q b c
                            | c == d = antg2 (ntg s) (ttc s) q b c
ntg' (Do (Init e) _)    b c          = O.ntg e b c
ntg' (Do _        s)    b c          = ntg s b c
ntg' S0                 _ _          = nan


-- | SSA of TTC. For Accel actions, transitivity is tried.
-- Situation argument is first for better currying inside the SSAs.
-- This is the memoizing function.
ttc :: RealFloat a => Sit (Prim a) -> Car -> Car -> TTC a
ttc = memo'' ttc'


-- | SSA of TTC. For Accel actions, transitivity is tried.
-- Situation argument is first for better currying inside the SSAs.
-- This is the non-memoized function doing the actual work.
ttc' :: RealFloat a => Sit (Prim a) -> Car -> Car -> NTG a
ttc' _                  b c | b == c = nan
ttc' (Do (Wait t) s)    b c          = tttc (ntg s) (ttc s) t b c
ttc' (Do (Accel d q) s) b c | b == d = orTrans
                                          (attc1 (ntg s) (ttc s) q)
                                          (ttcTrans (antg1 (ntg s) (ttc s) q)
                                                    (attc1 (ntg s) (ttc s) q))
                                          b c
                            | c == d = orTrans
                                          (attc2 (ntg s) (ttc s) q)
                                          (ttcTrans (antg2 (ntg s) (ttc s) q)
                                                    (attc2 (ntg s) (ttc s) q))
                                          b c
ttc' (Do (Init e) _)    b c          = O.ttc e b c
ttc' (Do _        s)    b c          = ttc s b c
ttc' S0                 _ _          = nan


memo' :: (Sit (Prim a) -> Car -> Car -> NTG a) ->
         (Sit (Prim a) -> Car -> Car -> NTG a)
memo' f = curry3 (memoOblivious stableNameFirstOfThree
                                hashStableNameFirstOfThree
                                (0,    minBound::Car, minBound::Car)
                                (2617, maxBound::Car, maxBound::Car)
                                (uncurry3 f))


memo'' :: (Sit (Prim a) -> Car -> Car -> NTG a) ->
          (Sit (Prim a) -> Car -> Car -> NTG a)
memo'' f = curry3 (memo stableNameAndHashFirstOfThree
                        (uncurry3 f))


nan :: RealFloat a => a
nan = (0 /) $! 0

