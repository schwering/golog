module Main (main) where

import System.Environment (getArgs)
import TORCS.Client
import TORCS.Golog.Driver

main :: IO ()
main = do args <- getArgs
          let host = if length args >= 1 then args !! 0 else "localhost"
          let port = if length args >= 2 then args !! 1 else "3001"
          run gologDriver host port

