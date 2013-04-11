{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import qualified Interpreter.GologTest
import qualified RSTC.TheoremsTest
import qualified Util.ExtFracTest
import qualified Util.InterpolationTest
import qualified Util.NativePSOTest
--import qualified Util.PSOTest
import Control.Monad (liftM2, ap)
import Test.QuickCheck.All

main :: IO ()
main = do passed <- return True
          passed <- liftM2 (&&) (return passed) Interpreter.GologTest.runTests
          passed <- liftM2 (&&) (return passed) RSTC.TheoremsTest.runTests
          passed <- liftM2 (&&) (return passed) Util.ExtFracTest.runTests
          passed <- liftM2 (&&) (return passed) Util.InterpolationTest.runTests
          passed <- liftM2 (&&) (return passed) Util.NativePSOTest.runTests
          --passed <- liftM2 (&&) (return passed) Util.PSOTest.runTests
          if passed then putStrLn "All tests passed."
                    else putStrLn "Some tests failed."
