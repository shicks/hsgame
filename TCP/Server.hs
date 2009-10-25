module TCP.Server ( startServer ) where

import Network ( withSocketsDo, listenOn, accept, Socket, PortID(PortNumber) )
import System.IO ( Handle, hSetBuffering, BufferMode(..) )
import Control.Concurrent ( Chan, forkIO )
import Control.Monad ( forever )

import TCP.Chan ( ShowRead, handle2chans )

listenForClients :: (ShowRead a, ShowRead b) =>
                    Socket -> (String -> Chan a -> Chan b -> IO ()) -> IO ()
listenForClients sock job =
    forever $ do (h,n,_) <- accept sock  -- ignoring the port number
                 hSetBuffering h LineBuffering
                 (i,o) <- handle2chans h
                 forkIO $ job n i o

startServer :: (ShowRead a, ShowRead b) =>
               Int -> (String -> Chan a -> Chan b -> IO ()) -> IO ()
startServer p job =
    withSocketsDo $ do s <- listenOn $ PortNumber $ fromIntegral p
                       listenForClients s job
