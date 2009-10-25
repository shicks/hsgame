module TCP.Server ( startServer ) where

import Network ( withSocketsDo, listenOn, accept, Socket, PortID(PortNumber) )
import System.IO ( hSetBuffering, BufferMode(..) )
import Control.Concurrent ( forkIO )
import Control.Monad ( forever )

import TCP.Chan ( ShowRead, Input, Output, handle2io )

listenForClients :: (ShowRead a, ShowRead b) =>
                    Socket -> (String -> Input a -> Output b -> IO ()) -> IO ()
listenForClients sock job =
    forever $ do (h,n,_) <- accept sock  -- ignoring the port number
                 hSetBuffering h LineBuffering
                 (i,o) <- handle2io h
                 forkIO $ job n i o

startServer :: (ShowRead a, ShowRead b) =>
               Int -> (String -> Input a -> Output b -> IO ()) -> IO ()
startServer p job =
    withSocketsDo $ do s <- listenOn $ PortNumber $ fromIntegral p
                       listenForClients s job
