module Main where

import System.Environment ( getArgs )
import System.IO ( hPutStrLn, hGetLine )
import Control.Monad ( forever )
import Control.Concurrent ( forkIO )

import TCP.Client ( connectToServer )

main :: IO ()
main = do [hostname] <- getArgs
          h <- connectToServer hostname 12345
          forkIO $ forever $ do putStrLn "Waiting for a line!"
                                y <- hGetLine h
                                putStrLn y
          forever $ do x <- getLine
                       hPutStrLn h x
