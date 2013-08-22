-- | Client loop for SCR robots.
-- An instance of the 'Driver' class should be handed to the 'run' function.
module TORCS.Client (Driver(..), run) where

import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (isNothing, fromJust)
import Network.Socket (Socket, SockAddr, SocketType(Datagram), addrAddress,
                       getAddrInfo, socket, addrFamily, sClose)
import Network.Socket.ByteString (sendAllTo, recvFrom)
import Network.BSD (HostName, defaultProtocol)
import System.Timeout
import TORCS.CarControl
import TORCS.CarState
import TORCS.Driver
import TORCS.MessageParser
import TORCS.PhysicsUtil

type Port = String
data Handle = Handle Socket SockAddr

-- | Socket timeout in microseconds (@10^-6 s@).
udpTimeout :: Int
udpTimeout = 1000000

createHandle :: HostName -> Port -> IO Handle
createHandle hostname port =
    do   addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
         let serveraddr = head addrinfos
         sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
         return $ Handle sock (addrAddress serveraddr)

closeHandle :: Handle -> IO ()
closeHandle (Handle sock _) = sClose sock

send :: Handle -> String -> IO ()
send (Handle sock addr) str = do sendAllTo sock (pack str) addr
                                 --putStrLn $ "SEND: " ++ str

recv :: Handle -> IO String
recv (Handle sock addr) = do  (bstr, addr') <- recvFrom sock 1000
                              if addr /= addr'
                                 then fail "Invalid sender address"
                                 else return ()
                              let str = unpack bstr
                              --putStrLn $ " RECV: " ++ str
                              return str

-- | Client loop for a driver @a@.
-- The 'HostName' and 'Port' identify the SCR competition server, i.e., the
-- TORCS process with a @scr_server@ robot.
-- For @scr_server 1@ the port should be @"3001"@, for @scr_server 2@ the port
-- should be @"3002"@ etc.
run :: Driver a => a -> HostName -> Port -> IO ()
run driver host port =
      do handle <- createHandle host port
         state <- initialState driver
         greet handle
         loop handle state
   where beamDegs = reverse (map rad2deg (beamOris driver))
         greet handle =
            do send handle $ name driver ++ stringify "init" beamDegs
               maybeStr <- timeout udpTimeout $ recv handle
               if maybe False ("***identified***\0" ==) maybeStr
                  then do  putStrLn "Indentified, starting loop"
                  else do  putStrLn "No response from SCR server yet"
                           greet handle
         loop handle state =
            do str <- recv handle
               if "***shutdown***\0" == str
                  then do  state' <- shutdown state
                           if isNothing state'
                              then do  closeHandle handle
                              else do  greet handle
                                       loop handle (fromJust state')
                  else
                     if "***restart***\0" == str
                        then do  state' <- restart state
                                 loop handle state'
                        else do  (state', ctrl) <- command state (parseState str)
                                 send handle (stringifyControl ctrl)
                                 loop handle state'

