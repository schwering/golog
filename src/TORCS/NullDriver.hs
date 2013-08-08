module TORCS.NullDriver where

import TORCS.Client
import TORCS.MessageParser

data NullDriver = NullDriver

instance Driver NullDriver where
   command _ s = do  putStrLn $ "COMMAND: " ++ s
                     return $ stringify1 "accel" 0.75
   shutdown _ = putStrLn $ "SHUTDOWN"
   restart _ = putStrLn $ "RESTART"

