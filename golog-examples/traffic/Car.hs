module Car where

import qualified Data.Map as Map
import Golog.Interpreter
import RoadMap (Direction)

type CarLUT a = Map.Map (a, a) (Car a)

type Car = Char

data A = Accel Car Double | Orientate Direction | Tick

instance BAT A where
   data Sit A = State { veloc       :: Int
                      , position    :: (Int, Int)
                      , orientation :: Direction
                      }
      deriving Show

   s0 = State { veloc = 0, position = (0,0), orientation = East }

   do_ (Accel 

