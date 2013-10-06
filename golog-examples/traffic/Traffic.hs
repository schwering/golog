module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.IORef
import Golog.Interpreter
import Golog.Macro
import Golog.Util
import World
import System.Environment (getArgs)
import Visualization

myStreets :: [Street Int]
myStreets = [ mkStreet "1" kblu [(75,10), (3,10), (3,3), (75,3), (75,10)]
            , mkStreet "A" kmag [(30,5), (30,20), (70,20), (70,5), (30,5)]
            --, mkStreet "2" kred [(12,0), (12,30)]
            ]

-- | Duration of a cycle in milliseconds.
cycleTime :: Int
cycleTime = 500 * 1000

lightSwitcher :: Ord a => IORef (World a) -> IO ()
lightSwitcher wRef = forever $ do w <- readIORef wRef
                                  writeIORef wRef (switchLights w)
                                  threadDelay (500 * 1000)

main :: IO ()
main = do
   args <- getArgs
   let width = if length args >= 1 then read (args !! 0) - 2 else 80
   let height = if length args >= 2 then read (args !! 1) - 2 else 30
   wRef <- newIORef $ mkWorld width height myStreets
   _ <- forkIO $ lightSwitcher wRef
   _ <- forever $ readIORef wRef >>= draw >> threadDelay cycleTime
   return ()

