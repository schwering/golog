{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- | Low-level BAT for perception.
-- Offline perception is done by simple simulation.
-- For that, however, we need the current 'CarControl', which is why the 'Sense'
-- action takes this as argument.
module TORCS.Golog.BAT.Perception where

import qualified Control.Concurrent.SSem as Sem
import Data.IORef
import Data.Maybe (fromMaybe)
import Golog.Interpreter
import TORCS.CarControl
import TORCS.CarState
import TORCS.Golog.Simulation

data A = Sense CarControl (Maybe CarState)

instance BAT A where
   data Sit A = Sit {
      cs       :: CarState,
      cc       :: CarControl,
      csRef    :: IORef CarState,
      ticker   :: Sem.SSem
   }

   s0 = Sit defaultState defaultControl undefined undefined

   do_ (Sense cc' mcs') s = s{cs = cs', cc = cc'}
      where cs' = fromMaybe (simulateState tickDurInSec cc' (cs s)) mcs'

   poss _ _ = True

instance IOBAT A IO where
   syncA (Sense cc' Nothing) s = do Sem.wait (ticker s)
                                    cs' <- readIORef (csRef s)
                                    return $ do_ (Sense cc' (Just cs')) s
   syncA (Sense _ _)         _ = error "syncA: unrefinable Sense"

tickDurInSec :: Double
tickDurInSec = 10 / 1000


