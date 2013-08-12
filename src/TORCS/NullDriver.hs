{-# LANGUAGE TypeFamilies #-}

module TORCS.NullDriver where

import TORCS.CarControl
import TORCS.CarState
import TORCS.Client

data NullDriver = NullDriver

instance Driver NullDriver where
   data State NullDriver = Dummy
   initialState _ = return Dummy
   command s state = do  putStrLn ("STATE: " ++ show state)
                         return (s, defaultControl{accel=0.75})
   shutdown _        = do  putStrLn "SHUTDOWN"
   restart state     = do  putStrLn "RESTART"
                           return state

