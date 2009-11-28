import TCP.Server ( startServer )
import Control.Monad ( forever )
import System.IO ( hPutStrLn, hGetLine )

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( newChan, writeChan, readChan, dupChan )

main = do c <- newChan
          forkIO $ forever $ getLine >>= writeChan c
          forkIO $ forever $ readChan c >>= putStrLn
          startServer 12345 $ \n h -> do c' <- dupChan c
                                         forkIO $ forever $ do l <- readChan c'
                                                               hPutStrLn h l
                                         forever $ do l <- hGetLine h
                                                      hPutStrLn h l
                                                      writeChan c' l
