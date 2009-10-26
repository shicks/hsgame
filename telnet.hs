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
