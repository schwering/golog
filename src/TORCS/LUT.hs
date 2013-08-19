{-# LANGUAGE ForeignFunctionInterface #-}

-- | Lookup-tables for angle and speed.

module TORCS.LUT (predictAngleDiff, predictSpeedXDiff) where

import Foreign.C

-- | Predicts the change of the 'angle' given a steering value and a duration in
-- seconds.
predictAngleDiff :: Double -> Double -> Double
predictAngleDiff steer time = x * (time / obsTime)
   where x = realToFrac (c_predict_angle_diff (realToFrac steer))

-- | Predicts the change of the 'speedX' given a current 'speedX', an 'accel'
-- and 'brake' command and a duration in seconds.
--predictSpeedXDiff :: Double -> Double -> Double -> Double -> Double
--predictSpeedXDiff speedX accel brake time = x * (time / obsTime)
--   where accelOrBrake = accel - brake
--         x = realToFrac (c_predict_speed_x_diff (realToFrac speedX) (realToFrac accelOrBrake))

predictSpeedXDiff :: Double -> Double -> Double -> Int -> Double -> Double
predictSpeedXDiff speedX accel brake gear time = x * (time / obsTime)
   where x = realToFrac (c_predict_speed_x_diff (realToFrac speedX) (realToFrac accel) (realToFrac brake) (fromIntegral gear))

-- | The time interval in seconds between the examples we learnt from.
obsTime :: Double
obsTime = 0.02

foreign import ccall unsafe "predict_angle_diff"
   c_predict_angle_diff :: CDouble -> CDouble -- steer -> angle_diff

foreign import ccall unsafe "predict_speed_x_diff"
   --c_predict_speed_x_diff :: CDouble -> CDouble -> CDouble -- speed_x -> accelOrBrake -> speed_x_diff
   c_predict_speed_x_diff :: CDouble -> CDouble -> CDouble -> CInt -> CDouble -- speed_x -> accel -> brake -> gear -> speed_x_diff

