{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Common base of BAT implementations using regression and progression.

module RSTC.BAT.Base (Prim(..), NTGCat(..), TTCCat(..),
                      State(..), HistState(..),
                      lookahead, ntgDiff, ttcDiff, quality, match,
                      bestAccel, ntgCats, ttcCats, nan,
                      defaultPoss, defaultReward,
                      remove, inject) where

import RSTC.Car
import Interpreter.Golog
import qualified RSTC.Obs as O
import RSTC.Theorems
import Util.Interpolation

import Data.List

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


-- | Class 'State' defines the RSTC fluents.
class RealFloat a => State a where
   time :: Sit (Prim a) -> Time a
   lane :: Sit (Prim a) -> Car -> Lane
   ntg  :: Sit (Prim a) -> Car -> Car -> NTG a
   ttc  :: Sit (Prim a) -> Car -> Car -> TTC a


-- | Class 'HistState' defines some common operations of situation-like types.
--
-- Minimal complete definition: history.
--
-- Note the list returned by 'history' and 'sit2list' differ in two points:
-- (1) 'history' is ordered latest action first, 'sit2list' is ordered first
-- action first.
-- (2) 'history' may be incomplete, while 'sit2list' should be complete.
-- (However, I don't know of a way to implement 'sit2list' without having a
-- complete 'history' without additional cost.)
class (State a, BAT (Prim a)) => HistState a where
   history  :: Sit (Prim a) -> [Prim a]
   histlen  :: Sit (Prim a) -> Int
   sitlen   :: Sit (Prim a) -> Int
   sit2list :: Sit (Prim a) -> [Prim a]
   list2sit :: [Prim a] -> Sit (Prim a)
   predsit  :: Sit (Prim a) -> Sit (Prim a)

   histlen  = length . history
   sitlen   = length . sit2list
   sit2list = reverse . history
   list2sit = append2sit s0
   predsit  = list2sit . init . sit2list


-- | Appends list of actions in given order to situation term as new actions.
append2sit :: HistState a => Sit (Prim a) -> [Prim a] -> Sit (Prim a)
append2sit s []     = s
append2sit s (a:as) = append2sit (do_ a s) as


-- | Injects a new action 'n' actions ago in the situation term.
inject :: HistState a => Int -> (Prim a) -> Sit (Prim a) -> Sit (Prim a)
inject n a s = list2sit (reverse (take n l ++ [a] ++ drop n l))
   where l = reverse (sit2list s)


-- | Removes the action 'n' actions ago in the situation term.
remove :: HistState a => Int -> Sit (Prim a) -> Sit (Prim a)
remove n s = list2sit (reverse (take n l ++ drop (n+1) l))
   where l = reverse (sit2list s)


bestAccel :: (Show a, HistState a) => Sit (Prim a) -> Car -> Car -> Accel a
bestAccel sit b c = if haveObs then 0.5 * fx + 0.5 * gx else nan
   where fx = let (f1, f2) = (f 2, f 3) in pick f1 f2 (nullAt id (canonicalize Recip  f1 0))
         gx = let (g1, g2) = (g 2, g 3) in pick g1 g2 (nullAt id (canonicalize Linear g1 0))
         pick f1 f2 xs = case sortBy (\x y -> compare (abs (f1 x + f2 x)) (abs (f1 y + f2 y))) xs of (x:_) -> x ; [] -> nan
         f n q = let s = (newSit n q)
                 in case history s of (Match e : _) -> ntgDiff s e b c
                                      _             -> nan
         g n q = let s = (newSit n q)
                 in case history s of (Match e : _) -> ntgDiff s e c b
                                      _             -> nan
         haveObs :: Bool
         haveObs = any (\a -> case a of { Match _ -> True ; _ -> False }) (history sit)
         newSit n q = append2sit sit (Accel b q : (obsActions n sit))
         obsActions :: (Show a, HistState a) => Int -> Sit (Prim a) -> [Prim a]
         obsActions n s'  = take (2*n) (nextObs (lastMatch s'))
         lastMatch :: HistState a => Sit (Prim a) -> O.Wrapper a
         lastMatch s' = lastMatch' (history s')
         lastMatch' :: [Prim a] -> O.Wrapper a
         lastMatch' (Match e : _ )  = O.wrap e
         lastMatch' (Init e  : _ )  = O.wrap e
         lastMatch' (_       : as)  = lastMatch' as
         lastMatch' []              = error "RSTC.BAT.Base.bestAccel: no init or match action"
         nextObs :: (Show a) => O.Wrapper a -> [Prim a]
         nextObs (O.Wrapper e) =
            case O.next e of Just e' -> Wait (O.time e' - O.time e) :
                                        Match e' :
                                        nextObs (O.wrap e')
                             Nothing -> []


defaultPoss :: HistState a => Prim a -> Sit (Prim a) -> Bool
defaultPoss (Wait t)             _ = t >= 0 && not (isNaN t)
defaultPoss a @ (Accel _ q)      s = noDupe a (history s) && not (isNaN q)
defaultPoss a @ (LaneChange b l) s = noDupe a (history s) && l /= lane s b
defaultPoss (Init _)             _ = True
defaultPoss (Prematch _)         _ = True
defaultPoss (Match e)            s = match e s
defaultPoss Abort                _ = False
defaultPoss NoOp                 _ = True
defaultPoss (Start _ _)          _ = True
defaultPoss (End _ _)            _ = True
defaultPoss (Msg _)              _ = True


defaultReward :: HistState a => Prim a -> Sit (Prim a) -> Reward
defaultReward (Wait _)         _ = 0
defaultReward (Accel _ _)      _ = -0.01
defaultReward (LaneChange _ _) _ = -0.01
defaultReward (Init _)         _ = 0
defaultReward (Prematch _)     _ = 0
defaultReward (Match e)        s = 1 - sum ntgDiffs / genericLength ntgDiffs
   where ntgDiffs = map realToFrac [abs (ntgDiff s e b c) | b <- cars, c <- cars, b /= c]
defaultReward Abort            _ = 0
defaultReward NoOp             _ = 0
defaultReward (Start _ _)      s = max 0 (1000 - 2 * (fromIntegral (histlen s)))
defaultReward (End _ _)        s = case dropWhile startOrEnd (history s) of (Match _ : _) -> 2 * (fromIntegral (histlen s - 1))
                                                                            _             -> 0
   where startOrEnd (Start _ _)  = True
         startOrEnd (End _ _)    = True
         startOrEnd _            = False
defaultReward (Msg _)          _ = 0


lookahead :: Depth
lookahead = 5


ntgDiff :: (State a, O.Obs a b) => Sit (Prim a) -> b -> Car -> Car -> a
ntgDiff s e b c = ntg s b c - O.ntg e b c


ttcDiff :: (State a, O.Obs a b) => Sit (Prim a) -> b -> Car -> Car -> a
ttcDiff s e b c = ttc s b c - O.ttc e b c


quality :: (State a, O.Obs a b) => Sit (Prim a) -> b -> Car -> Car -> a
quality = ntgDiff


match :: (State a, O.Obs a b) => b -> Sit (Prim a) -> Bool
match e s = let ntg_ttc = [(b, c, ntg s b c, O.ntg e b c,
                                  ttc s b c, O.ttc e b c) | b <- cars, c <- cars]
                ntgs  = [(ntg1, ntg2) | (b, c, ntg1, ntg2, _, _) <- ntg_ttc, b /= c]
                ttcs  = [(ttc1, ttc2, relVeloc' ntg1 ttc1, relVeloc' ntg2 ttc2) | (b, c, ntg1, ntg2, ttc1, ttc2) <- ntg_ttc, b < c]
                lanes = [(lane s b, O.lane e b) | b <- cars]
            in all (\(l1, l2) -> l1 == l2) lanes &&
               all (\(ntg1, ntg2) -> haveCommon (ntgCats ntg1)
                                                (ntgCats ntg2)) ntgs &&
               all (\(ttc1, ttc2, rv1, rv2) -> haveCommon (ttcCats ttc1 rv1)
                                                          (ttcCats ttc2 rv2)) ttcs
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


noDupe :: Prim a -> [Prim a] -> Bool
noDupe a' @ (Accel b _) (a:as) = case a of Wait _    -> True
                                           Accel c _ -> c < b
                                           _         -> noDupe a' as
noDupe a' @ (LaneChange b _) (a:as) = case a of Wait _         -> True
                                                LaneChange c _ -> c < b
                                                _              -> noDupe a' as
noDupe _ [] = True
noDupe _ _  = error "RSTC.BAT.Base.noDupe: neither Accel nor LaneChange"


nan :: RealFloat a => a
nan = (0 /) $! 0

