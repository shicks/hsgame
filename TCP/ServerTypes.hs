module TCP.ServerTypes ( Server, ServerModifier, ServerMessage(..), forkServer,
                         ioServer, pureServer,
                         ioConnector, modifyServer ) where

import Control.Monad ( forever )

import TCP.Chan ( Input, Output, pipe, getInput, writeOutput, readInput )
import TCP.Message ( Message )
import Control.Concurrent ( forkIO )

data ServerMessage a = M a | N

data Server agent messageToClient messageToServer =
    Server (Output (Message agent messageToClient) ->
            Input (Message agent messageToServer) -> IO ())

ioServer :: (Output (Message agent toclient)
             -> Input (Message agent toserver)
             -> IO ())
         -> Server agent toclient toserver
ioServer = Server

pureServer :: ([Message agent toserver] -> [Message agent toclient])
           -> Server agent toclient toserver
pureServer f = Server $ \o i -> do x <- getInput i
                                   mapM_ (writeOutput o) $ f x

forkServer :: Server agent toclient toserver
           -> IO (Input (Message agent toclient),
                  Output (Message agent toserver))
forkServer (Server f) = do (i,o) <- pipe
                           (i2,o2) <- pipe
                           forkIO $ f o i2
                           return (i, o2)

data ServerModifier up uptoclient uptoserver down downtoclient downtoserver =
    RC (Output (Message up uptoclient) ->
        Output (Message down downtoserver) ->
        Input (Either (Message up uptoserver)
                      (Message down downtoclient)) ->
        IO ())

ioConnector :: (Output (Message up uptoclient) ->
                Output (Message down downtoserver) ->
                Input (Either (Message up uptoserver)
                              (Message down downtoclient)) ->
                IO ())
            -> ServerModifier up uptoclient uptoserver
                              down downtoclient downtoserver
ioConnector = RC

modifyServer :: ServerModifier up uptoclient uptoserver
                               down downtoclient downtoserver
             -> Server down downtoclient downtoserver
             -> Server up uptoclient uptoserver
modifyServer (RC f) r = ioServer $ \oup iup ->
    do (idown,odown) <- forkServer r
       (iboth, oboth) <- pipe
       forkIO $ forever $ readInput idown >>= (writeOutput oboth . Right)
       forkIO $ forever $ readInput iup >>= (writeOutput oboth . Left)
       f oup odown iboth
