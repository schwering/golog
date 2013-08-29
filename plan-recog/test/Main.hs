{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad
import qualified RSTC.TheoremsTest
--import qualified Util.ExtFracTest
import qualified Util.InterpolationTest
import qualified Util.NativePSOTest
--import qualified Util.PSOTest
import System.Exit



main :: IO ()
main = do b <- liftM and $ sequence $ [RSTC.TheoremsTest.runTests
                                      --,Util.ExtFracTest.runTests
                                      ,Util.InterpolationTest.runTests
                                      ,Util.NativePSOTest.runTests
                                      --,Util.PSOTest.runTests
                                      ]
          if b then exitSuccess else exitFailure

