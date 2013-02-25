module RSTC.Car where

import Data.Ix
import System.IO.Unsafe

data Lane = LeftLane | RightLane deriving (Eq, Show)

data Car = A | B | C | D | E | F | G | H deriving (Bounded, Enum, Eq, Ix, Ord, Show)

cars :: [Car]
--cars = [B .. H]
cars = [D,H]


debug :: (Show a) => a -> a
debug x = unsafePerformIO (do putStrLn (show x)
                              return x)

