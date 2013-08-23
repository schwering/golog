-- | Control record for the SCR competition server.
module TORCS.CarControl where

import TORCS.MessageParser

-- | The commands for to be send to the SCR server.
-- Each field's comment is copied from the SCR manual.
data CarControl = CarControl {
      -- | Virtual gas pedal (0 means no gas, 1 full gas).
      -- Range @[0,1]@.
      accelCmd  :: Double,
      -- | Virtual brake pedal (0 means no brake, 1 full brake).
      -- Range @[0,1]@.
      brakeCmd  :: Double,
      -- | Virtual clutch pedal (0 means no clutch, 1 full clutch).
      -- Range @[0,1]@.
      clutchCmd :: Double,
      -- | Gear value.
      -- Range @{-1,0,1,...7}@.
      gearCmd   :: Int,
      -- | Steering value: -1 and +1 means respectively full right and left,
      -- that corresponds to an angle of 0.785398 rad.
      -- Range @[-1,1]@.
      steerCmd  :: Double,
      -- | Focus direction (see the focus sensors in Table 1) in degrees.
      -- Range @[-90,90]@.
      focusCmd  :: Int,
      -- | This is meta-control command: 0 do nothing, 1 ask competition server
      -- to restart the race.
      -- Range @[0,1]@.
      metaCmd   :: Int
   }
   deriving Show

stringifyControl :: CarControl -> String
stringifyControl ctrl = stringify1 "accel"  (accelCmd  ctrl) ++
                        stringify1 "brake"  (brakeCmd  ctrl) ++
                        stringify1 "clutch" (clutchCmd ctrl) ++
                        stringify1 "gear"   (gearCmd   ctrl) ++
                        stringify1 "steer"  (steerCmd  ctrl) ++
                        stringify1 "focus"  (focusCmd  ctrl) ++
                        stringify1 "meta"   (metaCmd   ctrl)

defaultControl :: CarControl
defaultControl = CarControl {
                    accelCmd  = 0,
                    brakeCmd  = 0,
                    clutchCmd = 0,
                    gearCmd   = 0,
                    steerCmd  = 0,
                    focusCmd  = 0,
                    metaCmd   = 0
                 }

