module Car where

data Lane = LeftLane | RightLane deriving (Eq, Show)

data Car = A | B | C | D | E | F | G | H deriving (Enum, Eq, Ord, Show)

cars :: [Car]
cars = [B .. H]

