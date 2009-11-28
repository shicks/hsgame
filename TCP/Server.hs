module TCP.Server ( startServer ) where

import Network ( withSocketsDo, listenOn, accept, Socket, PortID(PortNumber) )
import System.IO ( Handle, hSetBuffering, BufferMode(..) )
import Control.Concurrent ( forkIO )
import Control.Monad ( forever )

listenForClients :: Socket -> (String -> Handle -> IO ()) -> IO ()
listenForClients sock job =
    forever $ do (h,n,_) <- accept sock  -- ignoring the port number
                 hSetBuffering h LineBuffering
                 forkIO $ job n h

startServer :: Int -> (String -> Handle -> IO ()) -> IO ()
startServer p job =
    withSocketsDo $ do s <- listenOn $ PortNumber $ fromIntegral p
                       listenForClients s job
