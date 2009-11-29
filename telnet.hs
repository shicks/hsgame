||| Merge >>>
module Main where

import System.Environment ( getArgs )
import Control.Monad ( forever )
import Control.Concurrent ( forkIO )

import TCP.Client ( connectToServer )
import TCP.Chan ( writeOutput, readInput )

main :: IO ()
main = do [hostname] <- getArgs
          (i,o) <- connectToServer hostname 12345
          forkIO $ forever $ do putStrLn "Waiting for a line!"
                                y <- readInput i
                                putStrLn y
          forever $ do x <- getLine
                       writeOutput o x

<<< Merge |||
||| Merge stupidly? >>>
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

<<< Merge stupidly? |||
