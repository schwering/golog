{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module RSTC.Obs where

import Car
import RSTC.Theorems

import Foreign.C
import System.IO.Unsafe


class (RealFloat a) => Obs a b | b -> a where
   time :: b -> Time a
   ntg  :: b -> Car -> Car -> NTG a
   ttc  :: b -> Car -> Car -> TTC a
   lane :: b -> Car -> Lane


type ObsId = Int


instance Obs Double ObsId where
   time e     = unsafePerformIO (time_io e)
   ntg  e b c = unsafePerformIO (ntg_io e b c)
   ttc  e b c = unsafePerformIO (ttc_io e b c)
   lane e b   = unsafePerformIO (lane_io e b)


observations :: [Maybe ObsId]
observations = map (unsafePerformIO . next_obs_io) [0..]


next_obs_io :: ObsId -> IO (Maybe ObsId)
next_obs_io i = do res <- c_next_obs i'
                   if res == 1
                      then return (Just i)
                      else return Nothing
   where i' = fromIntegral i

time_io :: ObsId -> IO Double
time_io i = do res <- c_timestamp i'
               return (realToFrac res)
   where i' = fromIntegral i


lane_io :: ObsId -> Car -> IO Lane
lane_io i b = do res <- c_lane i' b'
                 if res == -1
                    then return LeftLane
                    else if res == 1
                            then return RightLane
                            else return RightLane -- some default (or Nothing?)
   where i' = fromIntegral i
         b' = fromIntegral (fromEnum b)


ntg_io :: ObsId -> Car -> Car -> IO Double
ntg_io i b c = do res <- c_ntg i' b' c'
                  return (realToFrac res)
   where i' = fromIntegral i
         b' = fromIntegral (fromEnum b)
         c' = fromIntegral (fromEnum c)


ttc_io :: ObsId -> Car -> Car -> IO Double
ttc_io i b c = do res <- c_ttc i' b' c'
                  return (realToFrac res)
   where i' = fromIntegral i
         b' = fromIntegral (fromEnum b)
         c' = fromIntegral (fromEnum c)


foreign import ccall "obs_next"
   c_next_obs :: CInt -> IO CInt -- obs_id -> success

foreign import ccall "obs_timestamp"
   c_timestamp :: CInt -> IO CDouble -- obs_id -> time

foreign import ccall "obs_lane"
   c_lane :: CInt -> CInt -> IO CInt -- obs_id -> -1=left, +1=right, 0=error

foreign import ccall "obs_ntg"
   c_ntg :: CInt -> CInt -> CInt -> IO CDouble -- obs_id -> b -> c -> NTG or NaN

foreign import ccall "obs_ttc"
   c_ttc :: CInt -> CInt -> CInt -> IO CDouble -- obs_id -> b -> c -> TTC or NaN

