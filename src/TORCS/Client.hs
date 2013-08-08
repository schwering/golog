-- | Client loop for SCR robots.
-- An instance of the 'Driver' class should be handed to the 'run' function.
module TORCS.Client (Driver(..), run) where

import Data.ByteString.Char8 (pack, unpack)
import Network.Socket (Socket, SockAddr, SocketType(Datagram), addrAddress, getAddrInfo, socket, addrFamily, sClose)
import Network.Socket.ByteString (sendAllTo, recvFrom)
import Network.BSD (HostName, defaultProtocol)
import TORCS.MessageParser

type Name = String
type Port = String
data Handle = Handle Socket SockAddr


-- | Class for SCR driver robots.
class Driver a where
   -- | Name should be @\"SCR\"@ unless changed in the SCR TORCS robot.
   name     :: a -> String

   -- | Orientation of the 19 range finders in degrees.
   -- Default: @[-90,-75,-60,-45,-30,20,15,10,5,0,5,10,15,20,30,45,60,75,90]@.
   angles   :: a -> [Double]

   -- | Handler for incoming sensor readings which returns a new action.
   -- The 
   command  :: a -> String -> IO String

   -- | Handler for the shutdown event.
   shutdown :: a -> IO ()

   -- | Handler for the restart event.
   restart  :: a -> IO ()

   name _   = "SCR"
   angles _ = [-90,-75,-60,-45,-30,20,15,10,5,0,5,10,15,20,30,45,60,75,90]


createHandle :: HostName -> Port -> IO Handle
createHandle hostname port =
    do addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
       return $ Handle sock (addrAddress serveraddr)


closeHandle :: Handle -> IO ()
closeHandle (Handle sock _) = sClose sock


send :: Handle -> String -> IO ()
send (Handle sock addr) str = do sendAllTo sock (pack str) addr
                                 putStrLn $ "SEND: " ++ str


recv :: Handle -> IO String
recv (Handle sock addr) = do  (bstr, addr') <- recvFrom sock 1000
                              if addr /= addr'
                                 then fail "Invalid sender address"
                                 else return ()
                              let str = unpack bstr
                              putStrLn $ " RECV: " ++ str
                              return str

-- | Client loop for a driver @a@.
-- The 'HostName' and 'Port' identify the SCR competition server, i.e., the
-- TORCS process with a @scr_server@ robot.
-- For @scr_server 1@ the port should be @"3001"@, for @scr_server 2@ the port
-- should be @"3002"@ etc.
run :: Driver a => a -> HostName -> Port -> IO ()
run driver host port =
      do h <- createHandle host port
         send h $ name driver ++ stringify "init" (angles driver)
         loop h
   where loop h = do str <- recv h
                     if str == "***shutdown***"
                        then do  shutdown driver
                        else
                           if str == "***restart***"
                              then do  restart driver
                                       loop h
                              else do  str' <- command driver str
                                       send h str'
                                       loop h

