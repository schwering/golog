{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Some physics-related types and utility functions.
--
-- By default we work with @m\/s@ and @rad@ units for speed and angles.
-- TORCS and SCR use @km\/h@ and @deg@ in some places. To avoid confusion, we
-- use special types for the latter.
module TORCS.PhysicsUtil where

newtype KmH = KmH { kmh :: Double }
   deriving (Num, Floating, Fractional, Real, RealFloat, RealFrac, Eq, Ord)

instance Read KmH where
   readsPrec i = map (\(x,s') -> (KmH x,s')) . readsPrec i

instance Show KmH where
   showsPrec i (KmH d) = showsPrec i d
   show (KmH d) = show d

kmh2ms :: KmH -> Double
kmh2ms (KmH kmh') = kmh' / 3.6

ms2kmh :: Double -> KmH
ms2kmh ms = KmH (ms * 3.6)


newtype Deg = Deg { deg :: Double }
   deriving (Num, Floating, Fractional, Real, RealFloat, RealFrac, Eq, Ord)

instance Read Deg where
   readsPrec i = map (\(x,s') -> (Deg x,s')) . readsPrec i

instance Show Deg where
   showsPrec i (Deg d) = showsPrec i d
   show (Deg d) = show d

deg2rad :: Deg -> Double
deg2rad (Deg deg') = deg' * 0.017453292519943295

rad2deg :: Double -> Deg
rad2deg rad = Deg (rad / 0.017453292519943295)

