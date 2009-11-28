module TCP.Client ( connectToServer ) where

import Network ( connectTo, PortID(PortNumber) )
import System.IO ( hSetBuffering, BufferMode(LineBuffering) )
import TCP.Chan ( ShowRead, Input, Output, handle2io )

connectToServer :: (ShowRead a, ShowRead b) =>
                   String -> Int -> IO (Input a, Output b)
connectToServer hostname port =
    do h <- connectTo hostname (PortNumber $ fromIntegral port)
       hSetBuffering h LineBuffering
       handle2io h
