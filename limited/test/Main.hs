{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad
import qualified ESLTest
import System.Exit

main :: IO ()
main = do b <- liftM and $ sequence $ [ESLTest.runTests
                                      ]
          when (not b) $ do putStrLn "Some tests failed!!!"
          if b then exitSuccess else exitFailure

