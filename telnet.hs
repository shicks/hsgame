||| add lobby-chat which supports tables. >>>
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

<<< add lobby-chat which supports tables. |||
||| reorganize actions in Game.hs >>>
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

<<< reorganize actions in Game.hs |||
