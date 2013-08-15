{-# LANGUAGE TypeFamilies #-}

-- | Client loop for SCR robots.
-- An instance of the 'Driver' class should be handed to the 'run' function.
module TORCS.Client (Driver(..), BeamOri(..), run, beamOriRad) where

import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (isNothing, fromJust)
import Network.Socket (Socket, SockAddr, SocketType(Datagram), addrAddress,
                       getAddrInfo, socket, addrFamily, sClose)
import Network.Socket.ByteString (sendAllTo, recvFrom)
import Network.BSD (HostName, defaultProtocol)
import System.Timeout
import TORCS.CarControl
import TORCS.CarState
import TORCS.MessageParser
import TORCS.PhysicsUtil

type Name = String
type Port = String
data Handle = Handle Socket SockAddr

data BeamOri =
   Neg90 | Neg75 | Neg60 | Neg45 | Neg30 | Neg20 | Neg15 | Neg10 | Neg5 | Zero |
   Pos5 | Pos10 | Pos15 | Pos20 | Pos30 | Pos45 | Pos60 | Pos75 | Pos90
   deriving (Show, Enum, Bounded, Eq, Ord)

beamOriRad :: BeamOri -> Double
beamOriRad Neg90 = deg2rad (-90)
beamOriRad Neg75 = deg2rad (-75)
beamOriRad Neg60 = deg2rad (-60)
beamOriRad Neg45 = deg2rad (-45)
beamOriRad Neg30 = deg2rad (-30)
beamOriRad Neg20 = deg2rad (-20)
beamOriRad Neg15 = deg2rad (-15)
beamOriRad Neg10 = deg2rad (-10)
beamOriRad Neg5  = deg2rad (-5)
beamOriRad Zero  = deg2rad 0
beamOriRad Pos5  = deg2rad 5
beamOriRad Pos10 = deg2rad 10
beamOriRad Pos15 = deg2rad 15
beamOriRad Pos20 = deg2rad 20
beamOriRad Pos30 = deg2rad 30
beamOriRad Pos45 = deg2rad 45
beamOriRad Pos60 = deg2rad 60
beamOriRad Pos75 = deg2rad 75
beamOriRad Pos90 = deg2rad 90

-- | Class for SCR driver robots.
class Driver a where
   data Context a :: *

   -- | Name should be @\"SCR\"@ unless changed in the SCR TORCS robot.
   -- The first argument is dummy.
   name         :: a -> Name

   -- | Orientation of the 19 range finders in degrees.
   -- The first argument is dummy.
   -- Default: the radians of the degrees in the range
   -- @[-90,-75,-60,-45,-30,20,15,10,5,0,5,10,15,20,30,45,60,75,90]@.
   beamOris     :: a -> [Double]

   -- | Returns the initial state.
   -- The first argument is dummy.
   initialState :: a -> IO (Context a)

   -- | Handler for incoming sensor readings which returns a new action.
   command      :: Context a -> CarState -> IO (Context a, CarControl)

   -- | Handler for the shutdown event.
   shutdown     :: Context a -> IO (Maybe (Context a))

   -- | Handler for the restart event.
   restart      :: Context a -> IO (Context a)

   name _     = "SCR"
   beamOris _ = map beamOriRad [minBound .. maxBound]

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

