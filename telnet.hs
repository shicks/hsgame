module Main where

import System.Environment ( getArgs )
import Control.Monad ( forever )
import Control.Concurrent ( forkIO )

import TCP.Client ( runClientTCP, ioClient )
import TCP.Chan ( writeOutput, readInput )

main :: IO ()
main = do [hostname] <- getArgs
          runClientTCP hostname 1234 $ ioClient $ \i o ->
              do forkIO $ forever $ readInput i >>= putStrLn
                 forever $ getLine >>= writeOutput o
