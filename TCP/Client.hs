module TCP.Client ( connectToServer ) where

import Network ( connectTo, PortID(PortNumber) )
import System.IO ( Handle, hSetBuffering, BufferMode(LineBuffering) )

connectToServer :: String -> Int -> IO Handle
connectToServer hostname port =
    do h <- connectTo hostname (PortNumber $ fromIntegral port)
       hSetBuffering h LineBuffering
       return h
