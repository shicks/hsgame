||| add lobby-chat which supports tables. >>>
module TCP.Client ( Client, runClientTCP, pureClient, ioClient, decoupledClient,
                    forkClient, ClientModifier, modifyClient,
                    ioClientModifier, pureClientModifier ) where

import Network ( connectTo, PortID(PortNumber) )
import System.IO ( hSetBuffering, BufferMode(LineBuffering) )
import Control.Concurrent ( forkIO )
import Control.Monad ( forever )

import TCP.Chan ( ShowRead, Input, Output, handle2io, pipe,
                  readInput, getInput, writeOutput )

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

decoupledClient :: (Input toclient -> IO ()) -> (Output toserver -> IO ())
                -> Client toclient toserver
decoupledClient reader writer = ioClient $ \i o ->
                                do forkIO $ reader i
                                   writer o

pureClient :: ([toclient] -> [toserver]) -> Client toclient toserver
pureClient f = ioClient $ \i o -> do x <- getInput i
                                     mapM_ (writeOutput o) $ f x

forkClient :: Client toclient toserver
           -> IO (Input toserver, Output toclient)
forkClient (Client f) = do (i,o) <- pipe
                           (i2,o2) <- pipe
                           forkIO $ f i2 o
                           return (i, o2)

data ClientModifier
 uptoclient uptoserver downtoclient downtoserver =
    CM (Output uptoserver ->
                 Output downtoclient ->
                 Input (Either uptoclient downtoserver) ->
                 IO ())

ioClientModifier :: (Output uptoserver ->
                     Output downtoclient ->
                     Input (Either uptoclient downtoserver) ->
                     IO ())
                 -> ClientModifier uptoclient uptoserver
                                   downtoclient downtoserver
ioClientModifier = CM

pureClientModifier :: ([Either uptoclient downtoserver] ->
                       [Either uptoserver downtoclient])
                   -> ClientModifier uptoclient uptoserver
                                     downtoclient downtoserver
pureClientModifier f = ioClientModifier $ \ouptoserver odowntoclient iboth ->
                       do xs <- getInput iboth
                          let os = f xs
                              writeit (Left m) = writeOutput ouptoserver m
                              writeit (Right m) = writeOutput odowntoclient m
                          mapM_ writeit os

modifyClient :: ClientModifier uptoclient uptoserver downtoclient downtoserver
             -> Client downtoclient downtoserver
             -> Client uptoclient uptoserver
modifyClient (CM f) c = ioClient $ \iup oup ->
    do (i,o) <- forkClient c
       (iboth, oboth) <- pipe
       forkIO $ forever $ do x <- readInput iup
                             writeOutput oboth (Left x)
       forkIO $ forever $ do x <- readInput i
                             writeOutput oboth (Right x)
       f oup o iboth

<<< add lobby-chat which supports tables. |||
||| reorganize actions in Game.hs >>>
module TCP.Client ( connectToServer ) where

import Network ( connectTo, PortID(PortNumber) )
import System.IO ( Handle, hSetBuffering, BufferMode(LineBuffering) )

connectToServer :: String -> Int -> IO Handle
connectToServer hostname port =
    do h <- connectTo hostname (PortNumber $ fromIntegral port)
       hSetBuffering h LineBuffering
       return h

<<< reorganize actions in Game.hs |||
