import TCP.Server ( startServer )
import Control.Monad ( forever )

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( newChan, writeChan, readChan, dupChan )

main :: IO ()
main = do c <- newChan
          forkIO $ forever $ getLine >>= writeChan c
          startServer 12345 $ \n i o ->
              do c' <- dupChan c
                 forkIO $ forever $ readChan c' >>= writeChan o
                 forever $ do l <- readChan i
                              writeChan o l
                              putStrLn $ n ++ ": " ++ l
