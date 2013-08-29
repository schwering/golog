{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad
import qualified TORCS.Golog.SimulationTest
import System.Exit

main :: IO ()
main = do b <- liftM and $ sequence $ [TORCS.Golog.SimulationTest.runTests]
          if b then exitSuccess else exitFailure
