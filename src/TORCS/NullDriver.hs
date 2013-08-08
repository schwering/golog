{-# LANGUAGE TypeFamilies #-}

module TORCS.NullDriver where

import TORCS.Client
import TORCS.MessageParser

data NullDriver = NullDriver

instance Driver NullDriver where
   data State NullDriver = Dummy
   initialState _ = return Dummy
   command state str = do  putStrLn ("COMMAND: " ++ str)
                           return (state, stringify1 "accel" 0.75)
   shutdown _        = do  putStrLn "SHUTDOWN"
   restart state     = do  putStrLn "RESTART"
                           return state

