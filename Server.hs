
module Server where

import Network ( withSocketsDo, listenOn, accept, Socket, PortID(PortNumber) )
import System.IO ( Handle, hPutStrLn, hGetLine,
                   hSetBuffering, BufferMode(..) )
import System.IO.Error ( catch )
import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef ( newIORef, readIORef, writeIORef )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( Chan, newChan, writeChan, readChan )
import Control.Monad ( forever )
import YAML

-- data Thread a = Thread {
--       inChan :: Chan a,
--       outChans :: M.Map String a
--     }

-- Tables need to
--   1. Listen to many sockets
--   2. Communicate with other stuff
-- we need 1 thread for each table

listenForClients :: Socket -> IO ()
listenForClients sock = forever $
                        do (h,n,p) <- accept sock
                           forkIO $ forever (hPutStrLn h "hello world")
                                    -- `catch` 
                           -- forkIO $ forever (hGetLine h >>= writeChan (inChan c))
                           --           `catch` disconnected


startServer :: Int -> IO ()
startServer p = withSocketsDo $ do s <- listenOn $ PortNumber $ fromIntegral p
                                   listenForClients s
