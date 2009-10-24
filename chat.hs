import TCP.Server ( startServer )
import Control.Monad ( forever )
import System.IO ( Handle, hPutStrLn, hGetLine, stdout )

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( newChan, writeChan, readChan, dupChan )

heSays :: Handle -> String -> (String, String) -> IO ()
heSays h me (him, statement)
       | him == me = return ()
       | otherwise = hPutStrLn h (him++" says: "++statement)

main :: IO ()
main = do c <- newChan
          forkIO $ forever $ do l <- getLine
                                writeChan c ("server",l)
          forkIO $ forever $ readChan c >>= heSays stdout "server"
          startServer 12345 $ \n h -> do c' <- dupChan c
                                         writeChan c' ("server",
                                                       "Everyone, welcome "++n)
                                         forkIO $ forever $
                                                readChan c' >>= heSays h n
                                         forever $ do l <- hGetLine h
                                                      writeChan c' (n,l)
