module Main (main) where

import System.Environment (getArgs)
import TORCS.Client
import TORCS.GologDriver

main :: IO ()
main = do args <- getArgs
          let args' = args ++ ["localhost", "3001"]
          run GologDriver (args' !! 0) (args' !! 1)

