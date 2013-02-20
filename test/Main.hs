{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import qualified Interpreter.GologTest
import qualified RSTC.TheoremsTest
import Control.Monad (liftM2, ap)
import Test.QuickCheck.All

main :: IO ()
main = liftM2 (&&) Interpreter.GologTest.runTests
                   RSTC.TheoremsTest.runTests
--main = Interpreter.GologTest.runTests
       >>= \passed -> if passed then putStrLn "All tests passed."
                                else putStrLn "Some tests failed."
