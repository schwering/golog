-- | Spatio-Temporal Reasoning about Traffic Scenarios.
-- Based on two temporal distance measures, net time gap (NTG) and time to
-- collision (TTC).
--
-- The following code implements the symmetry, transitivity, effect of by time,
-- and effect of acceleration on NTG and TTC.
--
-- We use floating point arithmetic. We omit all non-zero-checks! We use NaN and
-- infinity as representants for undefined values.
-- When NaN or infinity occurs, the callee should try to compute a valid value
-- via transitivity. The 'orTrans' function is helpful in this regard.

module RSTC.Theorems where

import Car

type NTG a = a
type TTC a = a
type Accel a = a
type Time a = a

type NTGF a = Car -> Car -> a
type TTCF a = Car -> Car -> a


-- | This function can back up a measure with transitive measures.
-- The first function is the direction measure (NTG or TTC), the second is the
-- transitive computation (via the middle car argument).
-- The function prefers non-NaN and non-infinity values; second preference is
-- infinity; third is NaN.
-- That is, when the direct measure is NaN or infinity, the transitive measure
-- is tried via some different car. This is repeated until `via' car is found
-- for which the transitive measure is non-NaN and non-infinity. When there is
-- none, but there is an infinity, infinity is returned. Otherwise NaN is
-- returned.
orTrans :: (RealFloat a) => (Car -> Car -> a) ->
                            (Car -> Car -> Car -> a) ->
                            Car -> Car -> a
orTrans f g b d = let viaCars    = [c | c <- cars, c /= b, c /= d]
                      direct     = f b d
                      transitive = map (\c -> g b c d) viaCars
                  in nonNaN direct (direct : transitive)
   where nonNaN y (x:xs) | isNaN x      = nonNaN y xs
                         | isInfinite x = nonNaN x xs
                         | otherwise    = x
         nonNaN y []                    = y



-- | Symmetry of NTG.
ntgSymm :: (RealFloat a) => NTGF a -> TTCF a -> Car -> Car -> NTG a
ntgSymm ntg ttc b c = -1 / (1 - (ntg b c) / (ttc b c)) * (ntg b c)


-- | Symmetry of TTC.
ttcSymm :: (RealFloat a) => NTGF a -> TTCF a -> Car -> Car -> TTC a
ttcSymm _ ttc b c = ttc c b


-- | Transitivity of NTG. The `via car is the third Car argument!
ntgTrans :: (RealFloat a) => NTGF a -> TTCF a -> Car -> Car -> Car -> NTG a
ntgTrans ntg ttc b c d = (ntg b c) + (1 - (ntg b c) / (ttc b c)) * (ntg c d)


-- | Transitivity of TTC. The `via car is the third Car argument!
ttcTrans :: (RealFloat a) => NTGF a -> TTCF a -> Car -> Car -> Car -> TTC a
ttcTrans ntg ttc b c d = lambda1 * (ttc b c) + lambda2 * (ttc c d)
   where lambda1 = (ttc c d) * (ntg b c) / denom
         lambda2 = ((ttc b c) * (ntg c d) - (ntg b c) * (ntg c d)) / denom
         denom   = (ntg c d) * (ttc b c) + (ttc c d) * (ntg b c) - (ntg c d) * (ntg b c)


-- | Temporal evolution of NTG.
tntg :: (RealFloat a) => NTGF a -> TTCF a -> Time a -> Car -> Car -> NTG a
tntg ntg ttc t b c = (ntg b c) - t * (ntg b c) / (ttc b c)


-- | Temporal evolution of TTC.
tttc :: (RealFloat a) => NTGF a -> TTCF a -> Time a -> Car -> Car -> TTC a
tttc _ ttc t b c = (ttc b c) - t


-- | Effect of acceleration of the first driver on NTG.
antg1 :: (RealFloat a) => NTGF a -> TTCF a -> Accel a -> Car -> Car -> NTG a
antg1 ntg _ q b c = 1 / q * (ntg b c)


-- | Effect of acceleration of the first driver on TTC.
attc1 :: (RealFloat a) => NTGF a -> TTCF a -> Accel a -> Car -> Car -> TTC a
attc1 ntg ttc q b c = (1 / ((q-1) * (ttc b c) / (ntg b c) + 1) * (ttc b c))


-- | Effect of acceleration of the second driver on NTG.
antg2 :: (RealFloat a) => NTGF a -> TTCF a -> Accel a -> Car -> Car -> NTG a
antg2 ntg _ _ b c = ntg b c


-- | Effect of acceleration of the second driver on TTC.
attc2 :: (RealFloat a) => NTGF a -> TTCF a -> Accel a -> Car -> Car -> TTC a
attc2 ntg ttc q b c = (1 / ((1-q) * (ttc b c) / (ntg b c) + q) * (ttc b c))


----------
-- Some useful properties.


-- | Ratio v(b) / v(c) where b is the first and c the second 'Car'.
relVeloc :: (RealFloat a) => NTGF a -> TTCF a -> Car -> Car -> a
relVeloc ntg ttc b c = 1 / (1 - (ntg b c) / (ttc b c))



data NTGRelation = IsFollowing
                 | IsFollowed
                 | IsApproaching
                 | IsMovingApart
                 | IsStanding
                 deriving (Eq, Show)

data TTCRelation = IsConverging
                 | IsDiverging
                 deriving (Eq, Show)


-- | What NTG tells us.
ntgRelation :: (RealFloat a) => NTGF a -> Car -> Car -> Maybe NTGRelation
ntgRelation ntg b c | isNaN (ntg b c) || isNaN (ntg c b) = Nothing
                    | ntg b c > 0 && ntg c b < 0         = Just IsFollowing
                    | ntg b c < 0 && ntg c b > 0         = Just IsFollowed
                    | ntg b c > 0 && ntg c b > 0         = Just IsApproaching
                    | ntg b c < 0 && ntg c b < 0         = Just IsMovingApart
                    | otherwise                          = Just IsStanding


-- | True iff b follows c.
isFollowing :: (RealFloat a) => NTGF a -> Car -> Car -> Bool
isFollowing ntg b c = ntgRelation ntg b c == Just IsFollowing


-- | True iff c follows b.
isFollowed :: (RealFloat a) => NTGF a -> Car -> Car -> Bool
isFollowed ntg b c = ntgRelation ntg b c == Just IsFollowed


-- | True iff b, c approach each other.
isApproaching :: (RealFloat a) => NTGF a -> Car -> Car -> Bool
isApproaching ntg b c = ntgRelation ntg b c == Just IsApproaching


-- | True iff b, c move apart.
isMovingApart :: (RealFloat a) => NTGF a -> Car -> Car -> Bool
isMovingApart ntg b c = ntgRelation ntg b c == Just IsMovingApart


-- | What TTC tells us.
ttcRelation :: (RealFloat a) => TTCF a -> Car -> Car -> Maybe TTCRelation
ttcRelation ttc b c | isNaN (ttc b c) = Nothing
                    | ttc b c > 0     = Just IsConverging
                    | ttc b c < 0     = Just IsDiverging
                    | otherwise       = Nothing


isConverging :: (RealFloat a) => TTCF a -> Car -> Car -> Bool
isConverging ttc b c = ttcRelation ttc b c == Just IsConverging


isDiverging :: (RealFloat a) => TTCF a -> Car -> Car -> Bool
isDiverging ttc b c = ttcRelation ttc b c == Just IsDiverging

