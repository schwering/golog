{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad
import qualified Golog.Old.InterpreterTest
import System.Exit

main :: IO ()
main = do b <- liftM and $ sequence $ [Golog.Old.InterpreterTest.runTests]
          if b then exitSuccess else exitFailure

