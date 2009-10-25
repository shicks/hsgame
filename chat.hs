import TCP.Server ( startServer )
import TCP.Chan ( readInput, Output, writeOutput, handle2o )

import Control.Monad ( forever )
import System.IO ( stdout )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( newChan, writeChan, readChan, dupChan )

heSaysC :: Output String -> String -> (String, String) -> IO ()
heSaysC h me (him, statement)
       | him == me = return ()
       | otherwise = writeOutput h (him++" says: "++statement)

main :: IO ()
main = do c <- newChan
          oo <- handle2o stdout
          forkIO $ forever $ do l <- getLine
                                writeChan c ("server",l)
          forkIO $ forever $ readChan c >>= heSaysC oo "server"
          startServer 12345 $ \hostname i o ->
              do c' <- dupChan c
                 writeOutput o "What is your name? "
                 n <- readInput i
                 writeChan c' ("server", "Everyone, welcome "++n++
                               " from "++hostname)
                 forkIO $ forever $ readChan c' >>= heSaysC o n
                 forever $ do l <- readInput i
                              writeChan c' (n,l)
