import TCP.Server ( startServer )
import Control.Monad ( forever )
import System.IO ( hPutStrLn, hGetLine )

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( newChan, writeChan, readChan, dupChan )

main = do c <- newChan
          forkIO $ forever $ getLine >>= writeChan c
          startServer 12345 $ \n h -> do c' <- dupChan c
                                         forkIO $ forever $ readChan c'
                                                            >>= hPutStrLn h
                                         forever $ do l <- hGetLine h
                                                      hPutStrLn h l
                                                      putStrLn $ n ++ ": " ++ l
