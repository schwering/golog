module Car where

import qualified Data.Map as Map

type CarLUT a = Map.Map (a, a) (Car a)

data Car a = Car { veloc    :: a
                 , position :: (a, a)
                 , symbol   :: Char
                 }
   deriving Show

