module HTTP.Server ( pure, httpServer ) where

import HTTP.Request ( Request(..), getRequest )
import HTTP.Response ( Response, sendResponse, error400 )

import Data.Char ( toLower )
import Control.Monad ( forever )
import Control.Concurrent ( forkIO )
import Network ( withSocketsDo, listenOn, accept, PortID(PortNumber) )
import Network.HTTP.Headers ( HeaderName(..), findHeader )
import System.IO ( hSetBuffering, hClose, Handle, BufferMode(..) )

-- type Server = Handle -> Request -> IO ()
-- type PureServer = Request -> IO Response

pure :: (Request -> IO Response) -> (Handle -> Request -> IO ())
pure f h r = f r >>= sendResponse h

httpServer :: Int -> (Handle -> Request -> IO ()) -> IO ()
httpServer port f =
    do sock <- listenOn $ PortNumber $ fromIntegral port
       withSocketsDo $ forever $ do
         (h,n,_) <- accept sock -- ignoring the port number
         hSetBuffering h LineBuffering
         forkIO $ openSocket n h
  where openSocket :: String -> Handle -> IO ()
        openSocket n h =
            do mreq <- getRequest n h
               case mreq of
                 Just req -> do
                           f h req
                           let conn = case (map toLower) `fmap`
                                           findHeader HdrConnection req of
                                        Just "close" -> False
                                        Just "keep-alive" -> True
                                        _ -> rqVersion req == "HTTP/1.1"
                           if conn then openSocket n h
                                   else hClose h
                 Nothing  -> error400 >>= sendResponse h >> hClose h
