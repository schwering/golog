{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import qualified Interpreter.GologTest
import Control.Monad (liftM2, ap)
import Test.QuickCheck.All

main :: IO ()
--main = liftM2 (&&) runTests Interpreter.GologTest.runTests
main = Interpreter.GologTest.runTests
       >>= \passed -> if passed then putStrLn "All tests passed."
                                else putStrLn "Some tests failed."
