{-# LANGUAGE ExistentialQuantification #-}

module RSTC.BAT where

import Car
import Interpreter.Golog
import qualified RSTC.Obs as O
import RSTC.Theorems

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
            deriving (Eq, Enum, Ord, Show)

data TTCCat = ConvergingFast
            | Converging
            | ConvergingSlowly
            | Reached
            | DivergingSlowly
            | Diverging
            | DivergingFast
            deriving (Eq, Enum, Ord, Show)


instance (RealFloat a) => BAT (Prim a) where
   poss (Wait t)             _ = t >= 0
   poss a @ (Accel _ _)      s = noDupe a s
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


match :: (RealFloat a, O.Obs a b) => b -> Sit (Prim a) -> Bool
match e s = let ntgs  = [(ntg s b c, O.ntg e b c) | b <- cars, c <- cars, b /= c]
                ttcs  = [(ttc s b c, O.ttc e b c) | b <- cars, c <- cars, b < c]
                lanes = [(lane b s, O.lane e b)   | b <- cars]
            in all (\(x, y) -> x == y) lanes &&
               all (\(x, y) -> haveCommon (ntgCats x) (ntgCats y)) ntgs &&
               all (\(x, y) -> haveCommon (ttcCats x) (ttcCats y)) ttcs
   where haveCommon (x:xs) (y:ys) | x < y     = haveCommon xs (y:ys)
                                  | y < x     = haveCommon (x:xs) ys
                                  | otherwise = True
         haveCommon []     _                  = False
         haveCommon _      []                 = False


ntgCats :: (RealFloat a) => NTG a -> [NTGCat]
ntgCats x = [cat | cat <- [VeryFarBehind .. VeryFarInfront], inCat cat ]
   where inCat VeryFarBehind    = 5 <= x
         inCat FarBehind        = 3 <= x && x <= 7
         inCat Behind           = 2 <= x && x <= 4
         inCat CloseBehind      = 1 <= x && x <= 2.5
         inCat VeryCloseBehind  = 0 <= x && x <= 1.5
         inCat SideBySide       = -0.75 <= x && x <= 0.75
         inCat VeryCloseInfront = -1.5 <= x && x <= 0
         inCat CloseInfront     = -2.5 <= x && x <= -1
         inCat Infront          = -4 <= x && x <= -2
         inCat FarInfront       = -7 <= x && x <= -3
         inCat VeryFarInfront   = x <= -5


ttcCats :: (RealFloat a) => TTC a -> [TTCCat]
ttcCats x = [cat | cat <- [ConvergingFast .. DivergingFast], inCat cat ]
   where inCat ConvergingFast   = 10 <= x
         inCat Converging       = 3.5 <= x && x <= 12
         inCat ConvergingSlowly = 0 <= x && x <= 5
         inCat Reached          = -2 <= x && x <= 2
         inCat DivergingSlowly  = -5 <= x && x <= 0
         inCat Diverging        = -12 <= x && x <= -3.5
         inCat DivergingFast    = x <= -10


noDupe :: Prim a -> Sit (Prim a) -> Bool
noDupe a' @ (Accel b _) (Do a s) = case a
   of Wait _         -> True
      Accel c _      -> c < b
      _              -> noDupe a' s
noDupe a' @ (LaneChange b _) (Do a s) = case a
   of Wait _         -> True
      LaneChange c _ -> c < b
      _              -> noDupe a' s
noDupe _ _ = error "RSTC.BAT.noDupe: neither Accel nor LaneChange"


----------
-- Successor State Axioms.


start :: (RealFloat a) => Sit (Prim a) -> Time a
start (Do (Wait t) s) = t + (start s)
start (Do _ s)        = start s
start S0              = 0

-- | SSA for lane.
lane :: (RealFloat a) => Car -> Sit (Prim a) -> Lane
lane b (Do (LaneChange c l) _) | b == c = l
lane b (Do (Init e) _)                  = O.lane e b
lane b (Do _ s)                         = lane b s
lane _ S0                               = RightLane


-- | SSA of NTG. For Accel actions, transitivity is tried.
-- Situation argument is first for better currying inside the SSAs.
ntg :: (RealFloat a) => Sit (Prim a) -> Car -> Car -> NTG a
ntg _                  b c | b == c = nan
ntg (Do (Wait t) s)    b c          = orTrans
                                          (tntg (ntg s) (ttc s) t)
                                          (ntgTrans (tntg (ntg s) (ttc s) t)
                                                    (tttc (ntg s) (ttc s) t))
                                          b c
ntg (Do (Accel d q) s) b c | b == d = antg1 (ntg s) (ttc s) q b c
                           | c == d = antg2 (ntg s) (ttc s) q b c
ntg (Do (Init e) _)    b c          = O.ntg e b c
ntg (Do _        s)    b c          = ntg s b c
ntg S0                 _ _          = nan


-- | SSA of TTC. For Accel actions, transitivity is tried.
-- Situation argument is first for better currying inside the SSAs.
ttc :: (RealFloat a) => Sit (Prim a) -> Car -> Car -> NTG a
ttc _                  b c | b == c = nan
ttc (Do (Wait t) s)    b c          = tttc (ntg s) (ttc s) t b c
ttc (Do (Accel d q) s) b c | b == d = orTrans
                                          (attc1 (ntg s) (ttc s) q)
                                          (ttcTrans (antg1 (ntg s) (ttc s) q)
                                                    (attc1 (ntg s) (ttc s) q))
                                          b c
                           | c == d = orTrans
                                          (attc2 (ntg s) (ttc s) q)
                                          (ttcTrans (antg2 (ntg s) (ttc s) q)
                                                    (attc2 (ntg s) (ttc s) q))
                                          b c
ttc (Do (Init e) _)    b c          = O.ttc e b c
ttc (Do _        s)    b c          = ttc s b c
ttc S0                 _ _          = nan


nan :: (RealFloat a) => a
nan = (0 /) $! 0

