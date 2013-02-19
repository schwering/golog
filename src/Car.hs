module Car where

data Car = B | C | D | E | F | G | H deriving (Enum, Eq, Ord, Show)

cars :: [Car]
cars = [B .. H]

