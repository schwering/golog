{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Observation interface.

module RSTC.Obs (Obs(..), Wrapper(..), wrap, ObsId, observations) where

import RSTC.Car
import RSTC.Theorems

import Foreign.C

class Obs a where
   next :: a -> Maybe a
   time :: RealFloat b => a -> Time b
   ntg  :: RealFloat b => a -> Car -> Car -> NTG b
   ttc  :: RealFloat b => a -> Car -> Car -> TTC b
   lane :: a -> Car -> Lane


data Wrapper = forall a. Obs a => Wrapper a

wrap :: Obs a => a -> Wrapper
wrap e = Wrapper e


newtype ObsId = ObsId Int

instance Show ObsId where
   show (ObsId n) = show n

instance Obs ObsId where
   next (ObsId n)     = getObs (ObsId (succ n))
   time (ObsId n)     = realToFrac (c_time (e2c n))
   ntg  (ObsId n) b c = realToFrac (c_ntg (e2c n) (c2c b) (c2c c))
   ttc  (ObsId n) b c = realToFrac (c_ttc (e2c n) (c2c b) (c2c c))
   lane (ObsId n) b   = case c_lane (e2c n) (c2c b) of -1 -> LeftLane
                                                       1  -> RightLane
                                                       _  -> RightLane


e2c :: Int -> CInt
e2c = fromIntegral

c2c :: Car -> CInt
c2c = fromIntegral . fromEnum


observations :: [Maybe ObsId]
observations = map (getObs . ObsId) [0..]


getObs :: ObsId -> Maybe ObsId
getObs (ObsId n) = case c_next_obs (e2c n) of 1 -> Just (ObsId n)
                                              _ -> Nothing

foreign import ccall unsafe "obs_next"
   c_next_obs :: CInt -> CInt -- obs_id -> success

foreign import ccall unsafe "obs_time"
   c_time :: CInt -> CDouble -- obs_id -> time

foreign import ccall unsafe "obs_lane"
   c_lane :: CInt -> CInt -> CInt -- obs_id -> -1=left, +1=right, 0=error

foreign import ccall unsafe "obs_ntg"
   c_ntg :: CInt -> CInt -> CInt -> CDouble -- obs_id -> b -> c -> NTG or NaN

foreign import ccall unsafe "obs_ttc"
   c_ttc :: CInt -> CInt -> CInt -> CDouble -- obs_id -> b -> c -> TTC or NaN

