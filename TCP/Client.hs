module TCP.Client ( runClientTCP, pureClient, ioClient ) where

import Network ( connectTo, PortID(PortNumber) )
import System.IO ( hSetBuffering, BufferMode(LineBuffering) )
import TCP.Chan ( ShowRead, Input, Output, handle2io, getInput, writeOutput )

runClientTCP :: (ShowRead a, ShowRead b) =>
                String -> Int -> Client a b -> IO ()
runClientTCP hostname port (Client client) =
    do h <- connectTo hostname (PortNumber $ fromIntegral port)
       hSetBuffering h LineBuffering
       (i,o) <- handle2io h
       client i o

data Client toclient toserver =
    Client (Input toclient -> Output toserver -> IO ())

ioClient :: (Input toclient -> Output toserver -> IO ())
         -> Client toclient toserver
ioClient = Client

pureClient :: ([toclient] -> [toserver]) -> Client toclient toserver
pureClient f = ioClient $ \i o -> do x <- getInput i
                                     mapM_ (writeOutput o) $ f x
