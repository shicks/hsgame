import TCP.Server ( startServer )

import Control.Monad ( forever )
import System.IO ( Handle, hPutStrLn, stdout )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( Chan, newChan, writeChan, readChan, dupChan )

heSaysC :: Chan String -> String -> (String, String) -> IO ()
heSaysC h me (him, statement)
       | him == me = return ()
       | otherwise = writeChan h (him++" says: "++statement)

heSays :: Handle -> String -> (String, String) -> IO ()
heSays h me (him, statement)
       | him == me = return ()
       | otherwise = hPutStrLn h (him++" says: "++statement)

main :: IO ()
main = do c <- newChan
          forkIO $ forever $ do l <- getLine
                                writeChan c ("server",l)
          forkIO $ forever $ readChan c >>= heSays stdout "server"
          startServer 12345 $ \hostname i o ->
              do c' <- dupChan c
                 writeChan o "What is your name? "
                 n <- readChan i
                 writeChan c' ("server", "Everyone, welcome "++n++
                               " from "++hostname)
                 forkIO $ forever $ readChan c' >>= heSaysC o n
                 forever $ do l <- readChan i
                              writeChan c' (n,l)
