-- | State record for the SCR competition server.
module TORCS.CarState where

import TORCS.MessageParser
import TORCS.PhysicsUtil

-- | The current state of the car, as received from the SCR server.
-- Each field's comment is copied from the SCR manual.
data CarState = CarState {
      -- | Angle between the car direction and the direction of the track axis.
      -- A positive angle indicates that the car is steering to the right.
      -- Range @[-pi,pi]@, unit radians.
      angle         :: Double,
      -- | Time elapsed during current lap.
      -- Range @[0,+inf)@, unit seconds.
      curLapTime    :: Double,
      -- | Current damage of the car (the higher is the value the higher is the
      -- damage).
      -- Range @[0,+inf)@, unit points.
      damage        :: Double,
      -- | Distance of the car from the start line along the track line.
      -- Range @[0,+inf)@, unit meters.
      distFromStart :: Double,
      -- | Distance covered by the car from the beginning of the race.
      -- Range @[0,+inf)@, unit meters.
      distRaced     :: Double,
      -- | Vector of 5 range finder sensors: each sensor returns the distance
      -- between the track edge and the car within a range of 200 meters.
      -- When noisy option is enabled (see Section 7) sensors are affected by
      -- i.i.d. normal noises with a standard deviation equal to the 1% of
      -- sensors range. The sensors sample, with a resolution of one degree, a
      -- five degree space along a specific direction provided by the client
      -- (the direction is defined with the focus command and must be in the
      -- range @[-pi\/2,+pi\/2]@ w.r.t. the car axis). Focus sensors are not
      -- always available: they can be used only once per second of simulated
      -- time. When the car is outside of the track (i.e., pos is less than -1
      -- or greater than 1), the focus direction is outside the allowed range
      -- (@[-pi\/2,+pi\/2]@) or the sensors has been already used once in the
      -- last second, the returned values are not reliable (typically -1 is
      -- returned).
      -- Range @[0,200]@, unit meters.
      focus         :: [Double],
      -- | Current fuel level.
      -- Range @[0,+inf)@, unit liters.
      fuel          :: Double,
      -- | Current gear: -1 is reverse, 0 is neutral and the gear from 1 to 7.
      -- Range @{-1,0,...,7}@, unit gears.
      gear          :: Int,
      -- | Time to complete the last lap.
      -- Range @[0,+inf)@, unit seconds.
      lastLapTime   :: Double,
      -- | Vector of 36 opponent sensors: each sensor covers a span of @pi\/18@
      -- (10 degrees) within a range of 200 meters and returns the distance of
      -- the closest opponent in the covered area. When noisy option is enabled
      -- (see Section 7), sensors are affected by i.i.d. normal noises with a
      -- standard deviation equal to the 2% of sensors range. The 36 sensors
      -- cover all the space around the car, spanning clockwise from @+pi@ up to
      -- @-pi@ with respect to the car axis.
      -- Range @[0,200]@, unit meters.
      -- NOTE: we reverse the order so that the beams start at @+pi@ and end at
      -- @-pi@, i.e., the beams go counter-clockwise.
      opponents     :: [Double],
      -- | Position in the race with respect to other cars.
      -- Range @{1,2,...}@, unit positions.
      racePos       :: Int,
      -- | Rumber of rotation per minute of the car engine.
      -- Range @[2000,7000]@, unit rpm.
      rpm           :: Double,
      -- | Speed of the car along the longitudinal axis of the car.
      -- Range @(-inf,+inf)@, unit m\/s.
      -- NOTE: the SCR bot sends km\/h, we convert it to m\/s at receive-time!
      speedX        :: Double,
      -- | Speed of the car along the transverse axis of the car.
      -- Range @(-inf,+inf)@, unit m\/s.
      -- NOTE: the SCR bot sends km\/h, we convert it to m\/s at receive-time!
      speedY        :: Double,
      -- | Speed of the car along the Z axis of the car.
      -- Range @(-inf,+inf)@, unit m\/s.
      -- NOTE: the SCR bot sends km\/h, we convert it to m\/s at receive-time!
      speedZ        :: Double,
      -- | Vector of 19 range finder sensors: each sensors returns the distance
      -- between the track edge and the car within a range of 200 meters. When
      -- noisy option is enabled (see Section 7), sensors are affected by i.i.d.
      -- normal noises with a standard deviation equal to the 10% of sensors
      -- range. By default, the sensors sample the space in front of the car
      -- every 10 degrees, spanning clockwise from @+pi\/2@ up to @-pi\/2@ with
      -- respect to the car axis. However, the configuration of the range finder
      -- sensors (i.e., the angle w.r.t. to the car axis) before the beginning
      -- of each race. When the car is outside of the track (i.e., pos is less
      -- than -1 or greater than 1), the returned values are not reliable.
      -- Range @[0,200]@, unit meters.
      track         :: [Double],
      -- | Distance between the car and the track axis. The value is normalized
      -- w.r.t to the track width: it is 0 when car is on the axis, -1 when the
      -- car is on the right edge of the track and +1 when it is on the left
      -- edge of the car. Values greater than 1 or smaller than -1 mean that the
      -- car is outside of the track.
      -- Range @(-inf,+inf)@, unit half track segment width.
      trackPos      :: Double,
      -- | Vector of 4 sensors representing the rotation speed of wheels.
      -- Range @[0,+inf)@, unit radians per second.
      wheelSpinVel  :: [Double],
      -- | Distance of the car mass center from the surface of the track along
      -- the Z axis.
      -- Range @(-inf,+inf)@, unit meters.
      z             :: Double
   }
   deriving Show

parseState :: String -> CarState
parseState str = CarState { angle          = parseMsg1 result "angle"
                          , curLapTime     = parseMsg1 result "curLapTime"
                          , damage         = parseMsg1 result "damage"
                          , distFromStart  = parseMsg1 result "distFromStart"
                          , distRaced      = parseMsg1 result "distRaced"
                          , focus          = parseMsg  result "focus"
                          , fuel           = parseMsg1 result "fuel"
                          , gear           = parseMsg1 result "gear"
                          , lastLapTime    = parseMsg1 result "lastLapTime"
                          , opponents      = reverse $ parseMsg  result "opponents"
                          , racePos        = parseMsg1 result "racePos"
                          , rpm            = parseMsg1 result "rpm"
                          , speedX         = kmh2ms $ parseMsg1 result "speedX"
                          , speedY         = kmh2ms $ parseMsg1 result "speedY"
                          , speedZ         = kmh2ms $ parseMsg1 result "speedZ"
                          , track          = parseMsg  result "track"
                          , trackPos       = parseMsg1 result "trackPos"
                          , wheelSpinVel   = parseMsg  result "wheelSpinVel"
                          , z              = parseMsg1 result "z"
                          }
   where result = parseMsg' str

defaultState :: CarState
defaultState = CarState {
                  angle          = 0,
                  curLapTime     = 0,
                  damage         = 0,
                  distFromStart  = 0,
                  distRaced      = 0,
                  focus          = replicate 5 (-1),
                  fuel           = 0,
                  gear           = 1,
                  lastLapTime    = 0,
                  opponents      = replicate 36 200,
                  racePos        = 1,
                  rpm            = 0,
                  speedX         = 0,
                  speedY         = 0,
                  speedZ         = 0,
                  track          = replicate 19 200,
                  trackPos       = 0.33,
                  wheelSpinVel   = replicate 4 0,
                  z              = 0
               }

