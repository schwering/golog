{-# LANGUAGE ExistentialQuantification #-}

-- | Basic action theory based on relative temporal measures.
--
-- The two measures are net time gap (NTG) and time to collision (TTC).
-- The successor state axioms (SSAs) use the 'RSTC.Theorems' module.

module RSTC.BAT where

import RSTC.Car
import Interpreter.Golog
import qualified RSTC.Obs as O
import RSTC.Theorems
import Util.Interpolation
import Util.MemoCache

import Data.List
import Data.Maybe

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

----------
-- Precondition and reward.

instance (RealFloat a, Show a) => BAT (Prim a) where
   poss (Wait t)             _ = t >= 0 && not (isNaN t)
   poss a @ (Accel _ q)      s = noDupe a s && not (isNaN q)
   poss a @ (LaneChange b l) s = noDupe a s && l /= lane s b
   poss (Init _)             _ = True
   poss (Prematch _)         _ = True
   poss (Match e)            s = match e s
   poss Abort                _ = False
   poss NoOp                 _ = True
   poss (Start _ _)          _ = True
   poss (End _ _)            _ = True
   poss (Msg _)              _ = True

   reward (Wait _)         _                = 0
   reward (Accel _ _)      _                = -0.01
   reward (LaneChange _ _) _                = -0.01
   reward (Init _)         _                = 0
   reward (Prematch _)     _                = 0
   reward (Match _)        _                = 1
   reward Abort            _                = 0
   reward NoOp             _                = 0
   reward (Start _ _)      s                = max 0 (1000 - 2 * (fromIntegral (sitlen s)))
   reward (End _ _)        (Do (Match _) s) = 2 * (fromIntegral (sitlen s))
   reward (End _ _)        _                = 0
   reward (Msg _)          _                = 0


-- | Lookahead commonly used in this BAT.
lookahead :: Depth
lookahead = 5


-- | Number of actions in a situation term.
sitlen :: Sit a -> Int
sitlen (Do _ s) = 1 + (sitlen s)
sitlen S0       = 0


-- | List of actions in situation term, ordered by their occurrence.
sit2list :: Sit a -> [a]
sit2list S0 = []
sit2list (Do a s) = (sit2list s) ++ [a]


-- | Situation term from the actions in list.
list2sit :: [a] -> Sit a
list2sit = append2sit S0


-- | Appends list of actions in given order to situation term as new actions.
append2sit :: Sit a -> [a] -> Sit a
append2sit s [] = s
append2sit s (a:as) = append2sit (Do a s) as


-- | Injects a new action 'n' actions ago in the situation term.
inject :: Int -> a -> Sit a -> Sit a
inject 0 a s         = Do a s
inject n a (Do a' s) = Do a' (inject (n-1) a s)
inject _ a S0        = Do a S0


-- | Renives the action 'n' actions ago in the situation term.
remove :: Int -> Sit a -> Sit a
remove 0 (Do _ s) = s
remove n (Do a s) = Do a (remove (n-1) s)
remove _ S0       = S0


ntgDiff :: (RealFloat a, O.Obs a b) => Sit (Prim a) -> b -> Car -> Car -> a
ntgDiff s e b c = ntg s b c - O.ntg e b c


ttcDiff :: (RealFloat a, O.Obs a b) => Sit (Prim a) -> b -> Car -> Car -> a
ttcDiff s e b c = ttc s b c - O.ttc e b c


quality :: (RealFloat a, O.Obs a b) => Sit (Prim a) -> b -> Car -> Car -> a
quality = ntgDiff


-- | Looks for the next Prematch action and returns the quality of this
-- situation compared to this observation.
valueByQuality :: RealFloat a => Car -> Car -> Depth -> SitTree Grown (Prim a) -> Maybe (a, a)
valueByQuality b c l t = val (best def max' cut l t)
   where val (Do (Prematch e) s, _, _, _)     = Just (quality s e b c, quality s e c b)
         val (_,                 _, _, _)     = Nothing
         cut (_,                 _, _, Final) = True
         cut (Do (Prematch _) _, _, _, _)     = True
         cut (_,                 _, _, _)     = False
         def      = (S0, 0, 0, Nonfinal)
         max' x y = case (val x, val y) of
                         -- The first case avoids that after Prematch actions
                         -- subsequent actions are evaluated (which could be
                         -- very expense if the lookahead leads the next pick
                         -- operator to being evaluated -> combinatorial
                         -- explosion).
                         (Just (x1, x2), _)             | abs x1 > 0      && abs x2 > 0      -> x
                         (Just (x1, x2), Just (y1, y2)) | abs x1 > abs y1 && abs x2 > abs y2 -> x
                                                        | otherwise                          -> y
                         (Just _,        Nothing)                                            -> x
                         _                                                                   -> y


bestAccel :: RealFloat a => Sit (Prim a) -> Car -> Car -> Accel a
bestAccel s b c = 0.5 * nullAt id (canonicalizeRecip f 0) +
                  0.5 * nullAt id (canonicalize      g 0)
   where lookahead = 2
         f q = case ss lookahead q of
                    s @ (Do (Match e) _) -> quality s e b c
                    _                    -> nan
         g q = case ss lookahead q of
                    s @ (Do (Match e) _) -> quality s e c b
                    _                    -> nan
         ss n q = append2sit (Do (Accel b q) s) (obsActions s !! n)
         obsActions (Do (Match e) _)  = inits (obsActions' (O.wrap e))
         obsActions (Do (Init e)  _)  = inits (obsActions' (O.wrap e))
         obsActions (Do _         s') = obsActions s'
         obsActions' :: O.Wrapper a -> [Prim a]
         obsActions' (O.Wrapper e) =
            case O.next e of Just e' -> Wait (O.time e' - O.time e) :
                                        Match e' :
                                        obsActions' (O.wrap e')
                             Nothing -> []


match :: (RealFloat a, O.Obs a b, Show a) => b -> Sit (Prim a) -> Bool
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


-- | SSA for starting time of situation.
-- This is the memoizing function.
start :: RealFloat a => Sit (Prim a) -> Time a
--start = memo1 1 start'
start = start'


-- | SSA for starting time of situation.
-- This is the non-memoized function doing the actual work.
start' :: RealFloat a => Sit (Prim a) -> Time a
start' (Do (Wait t) s) = t + (start s)
start' (Do _ s)        = start s
start' S0              = 0


-- | SSA for lane.
-- This is the memoizing function.
lane :: RealFloat a => Sit (Prim a) -> Car -> Lane
--lane = memo2 lane'
lane = lane'


-- | SSA for lane.
-- This is the non-memoized function doing the actual work.
lane' :: RealFloat a => Sit (Prim a) -> Car -> Lane
lane' (Do (LaneChange c l) _) b | b == c = l
lane' (Do (Init e) _)         b          = O.lane e b
lane' (Do _ s)                b          = lane s b
lane' S0                      _          = RightLane


-- | SSA of NTG. For Accel actions, transitivity is tried.
-- Situation argument is first for better currying inside the SSAs.
-- This is the memoizing function.
ntg :: RealFloat a => Sit (Prim a) -> Car -> Car -> NTG a
ntg = memo3 1 ntg'


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
ttc = memo3 2 ttc'


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


{-
memo' :: (Sit (Prim a) -> Car -> Car -> b) ->
         (Sit (Prim a) -> Car -> Car -> b)
memo' f = curry3 (memoOblivious stableName1of3
                                hashStableName1of3
                                (0,    minBound::Car, minBound::Car)
                                (2617, maxBound::Car, maxBound::Car)
                                (uncurry3 f))


memo3 :: (Sit (Prim a) -> Car -> Car -> b) ->
          (Sit (Prim a) -> Car -> Car -> b)
memo3 f = curry3 (memoMap stable (uncurry3 f))
   where stable = stableNameAndHash1of3


memo2 :: (Sit (Prim a) -> Car -> b) ->
         (Sit (Prim a) -> Car -> b)
memo2 f = curry (memoMap stable (uncurry f))
   where stable = stableNameAndHash1of2


memo1 :: (Sit (Prim a) -> b) ->
         (Sit (Prim a) -> b)
memo1 f = memoMap stable f
   where stable = stableNameAndHash
-}

{-
memo3 :: (Sit (Prim a) -> Car -> Car -> a) ->
          (Sit (Prim a) -> Car -> Car -> a)
memo3 f = curry3 (memoIntMap stable hash (uncurry3 f))
   where stable = stableName1of3
         hash = (\(x,y,z) -> x `combine` fromEnum y `combine` fromEnum z) . hashStableName1of3


memo2 :: (Sit (Prim a) -> Car -> Lane) ->
         (Sit (Prim a) -> Car -> Lane)
memo2 f = curry (memoIntMap stable hash (uncurry f))
   where stable = stableName1of2
         hash = (\(x,y) -> x `combine` fromEnum y) . hashStableName1of2


memo1 :: (Sit (Prim a) -> b) ->
         (Sit (Prim a) -> b)
memo1 f = memoMap stable hash f
   where stable = stableName
         hash = hashStableName
-}

nan :: RealFloat a => a
nan = (0 /) $! 0

