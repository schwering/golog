module Main (main) where

import System.Environment (getArgs)
import TORCS.Client
import TORCS.Golog.Driver

main :: IO ()
main = do args <- getArgs
          let args' = args ++ ["localhost", "3001"]
          run (gologDriver) (args' !! 0) (args' !! 1)

