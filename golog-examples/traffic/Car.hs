{-# LANGUAGE TypeFamilies #-}

module Car where

import Golog.Interpreter
import World (Direction)

type Car = Char

data A = Accel Car Double | Orientate Direction | Tick

instance BAT A where
   data Sit A = State { veloc       :: Int
                      , position    :: (Int, Int)
                      , orientation :: Direction
                      }
      deriving Show

   s0 = undefined -- State { veloc = 0, position = (0,0), orientation = East }

   do_ = undefined

