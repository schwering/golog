-- | Client loop for SCR robots.
-- An instance of the 'Driver' class should be handed to the 'run' function.
module TORCS.Client (Driver(..), run) where

import Control.Monad (when)
import Data.ByteString.Char8 (pack, unpack)
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
   do addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
      let serveraddr = head addrinfos
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      return $ Handle sock (addrAddress serveraddr)

closeHandle :: Handle -> IO ()
closeHandle (Handle sock _) = sClose sock

send :: Handle -> String -> IO ()
send (Handle sock addr) str = do sendAllTo sock (pack str) addr
                                 --putStrLn $ "SEND: " ++ str

recv :: Handle -> IO String
recv (Handle sock addr) = do  (bstr, addr') <- recvFrom sock 4096
                              when (addr /= addr') $ fail "Invalid sender address"
                              let str = unpack bstr
                              --putStrLn $ " RECV: " ++ str
                              return str

cases :: Eq a => a -> [(a, b)] -> b
cases _ []                        = error "cases: empty list"
cases _ [(_, y)]                  = y
cases z ((x,y):ps) | x == z   = y
                    | otherwise = cases z ps

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
               cases str
                  [("***shutdown***\0",
                     shutdown state >>= maybe (closeHandle handle) (\state' -> greet handle >> loop handle state'))
                  ,("***restart***\0",
                     restart state >>= loop handle)
                  ,(undefined,
                     do (state', ctrl) <- command state (parseState str)
                        send handle (stringifyControl ctrl)
                        loop handle state')
                  ]

