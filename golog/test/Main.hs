{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad
import qualified Golog.InterpreterTest
import qualified Golog.MacroTest
import System.Exit

main :: IO ()
main = do b <- liftM and $ sequence $ [Golog.InterpreterTest.runTests
                                      ,Golog.MacroTest.runTests
                                      ]
          when (not b) $ do putStrLn "Some tests failed!!!"
          if b then exitSuccess else exitFailure

