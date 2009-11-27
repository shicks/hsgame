module TCP.Router ( Router, RouterConnector, RouterMessage(..), forkRouter,
                    ioRouter, pureRouter,
                    ioConnector, connectRouter ) where

import Control.Monad ( forever )

import TCP.Chan ( Input, Output, pipe, getInput, writeOutput, readInput )
import TCP.Message ( Message )
import Control.Concurrent ( forkIO )

data RouterMessage a = M a | N

data Router agent messageToClient messageToServer =
    Router (Output (Message agent messageToClient) ->
            Input (Message agent messageToServer) -> IO ())

ioRouter :: (Output (Message agent toclient)
             -> Input (Message agent toserver)
             -> IO ())
         -> Router agent toclient toserver
ioRouter = Router

pureRouter :: ([Message agent toserver] -> [Message agent toclient])
           -> Router agent toclient toserver
pureRouter f = Router $ \o i -> do x <- getInput i
                                   mapM_ (writeOutput o) $ f x

forkRouter :: Router agent toclient toserver
           -> IO (Input (Message agent toclient),
                  Output (Message agent toserver))
forkRouter (Router f) = do (i,o) <- pipe
                           (i2,o2) <- pipe
                           forkIO $ f o i2
                           return (i, o2)

data RouterConnector up upmess down downmess =
    RC (Output (Message up upmess) ->
        Output (Message down (RouterMessage downmess)) ->
        Input (Either (Message up (RouterMessage upmess))
                      (Message down downmess)) ->
        IO ())

ioConnector :: (Output (Message up upmess) ->
                Output (Message down (RouterMessage downmess)) ->
                Input (Either (Message up (RouterMessage upmess))
                       (Message down downmess)) ->
                IO ())
            -> RouterConnector up upmess down downmess
ioConnector = RC

connectRouter :: RouterConnector up upmess down downmess
              -> Router down downmess (RouterMessage downmess)
              -> Router up upmess (RouterMessage upmess)
connectRouter (RC f) r = ioRouter $ \oup iup ->
    do (idown,odown) <- forkRouter r
       (iboth, oboth) <- pipe
       forkIO (f oup odown iboth)
       forkIO $ forever $ readInput idown >>= (writeOutput oboth . Right)
       forkIO $ forever $ readInput iup >>= (writeOutput oboth . Left)
       return ()
