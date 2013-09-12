{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad
import qualified Golog.InterpreterTest
import qualified Golog.MacroTest
import System.Exit

main :: IO ()
main = do b <- liftM and $ sequence $ [Golog.InterpreterTest.runTests]
          b <- liftM and $ sequence $ [Golog.MacroTest.runTests]
          if b then exitSuccess else exitFailure

