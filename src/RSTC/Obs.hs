{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module RSTC.Obs where

import Car
import RSTC.Theorems

import Foreign.C


class (RealFloat a) => Obs a b | b -> a where
   time :: b -> Time a
   ntg  :: b -> Car -> Car -> NTG a
   ttc  :: b -> Car -> Car -> TTC a
   lane :: b -> Car -> Lane


type ObsId = Int


instance Obs Double ObsId where
   time e     = realToFrac (c_timestamp (e2c e))
   ntg  e b c = realToFrac (c_ntg (e2c e) (c2c b) (c2c c))
   ttc  e b c = realToFrac (c_ttc (e2c e) (c2c b) (c2c c))
   lane e b   = case c_lane (e2c e) (c2c b) of -1 -> LeftLane
                                               1  -> RightLane
                                               _  -> RightLane


e2c = fromIntegral
c2c = fromIntegral . fromEnum



observations :: [Maybe ObsId]
observations = map next_obs_io [0..]


next_obs_io :: ObsId -> Maybe ObsId
next_obs_io i = case c_next_obs (fromIntegral i) of 1 -> Just i
                                                    _ -> Nothing


foreign import ccall unsafe "obs_next"
   c_next_obs :: CInt -> CInt -- obs_id -> success

foreign import ccall unsafe "obs_timestamp"
   c_timestamp :: CInt -> CDouble -- obs_id -> time

foreign import ccall unsafe "obs_lane"
   c_lane :: CInt -> CInt -> CInt -- obs_id -> -1=left, +1=right, 0=error

foreign import ccall unsafe "obs_ntg"
   c_ntg :: CInt -> CInt -> CInt -> CDouble -- obs_id -> b -> c -> NTG or NaN

foreign import ccall unsafe "obs_ttc"
   c_ttc :: CInt -> CInt -> CInt -> CDouble -- obs_id -> b -> c -> TTC or NaN

