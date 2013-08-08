{-# LANGUAGE DeriveDataTypeable #-}

-- | Control record for the SCR competition server.
module TORCS.CarControl where

import Data.Typeable
import TORCS.MessageParser

-- | The commands for to be send to the SCR server.
-- Each field's comment is copied from the SCR manual.
data CarControl = CarControl {
      -- | Virtual gas pedal (0 means no gas, 1 full gas).
      -- Range @[0,1]@.
      accel    :: Double,
      -- | Virtual brake pedal (0 means no brake, 1 full brake).
      -- Range @[0,1]@.
      brake    :: Double,
      -- | Virtual clutch pedal (0 means no clutch, 1 full clutch).
      -- Range @[0,1]@.
      clutch   :: Double,
      -- | Gear value.
      -- Range @{-1,0,1,...7}@.
      gear     :: Int,
      -- | Steering value: -1 and +1 means respectively full right and left,
      -- that corresponds to an angle of 0.785398 rad.
      -- Range @[-1,1]@.
      steering :: Double,
      -- | Focus direction (see the focus sensors in Table 1) in degrees.
      -- Range @[-90,90]@.
      focus    :: Int,
      -- | This is meta-control command: 0 do nothing, 1 ask competition server
      -- to restart the race.
      -- Range @[0,1]@.
      meta     :: Int
   }
   deriving (Typeable, Show)

stringifyControl :: CarControl -> String
stringifyControl ctrl = stringify1 "accel"    (accel ctrl) ++
                        stringify1 "brake"    (brake ctrl) ++
                        stringify1 "clutch"   (clutch ctrl) ++
                        stringify1 "gear"     (gear ctrl) ++
                        stringify1 "steering" (steering ctrl) ++
                        stringify1 "focus"    (focus ctrl) ++
                        stringify1 "meta"     (meta ctrl)

