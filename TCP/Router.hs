module TCP.Router ( Router, RouterConnector, RouterMessage(..), forkRouter,
                    ioRouter, pureRouter,
                    ioConnector, connectRouter ) where

import Control.Monad ( forever )

import TCP.Chan ( Input, Output, pipe, getInput, writeOutput, readInput )
import TCP.Message ( Message )
import Control.Concurrent ( forkIO )

data RouterMessage a = M a | N

data Router agent message =
    Router (Output (Message agent message) ->
            Input (Message agent (RouterMessage message)) -> IO ())

ioRouter :: (Output (Message agent message)
             -> Input (Message agent (RouterMessage message))
             -> IO ())
         -> Router agent message
ioRouter = Router

pureRouter :: ([Message agent (RouterMessage message)]
                   -> [Message agent message]) -> Router agent message
pureRouter f = Router $ \o i -> do x <- getInput i
                                   mapM_ (writeOutput o) $ f x

forkRouter :: Router agent message
           -> IO (Input (Message agent message),
                  Output (Message agent (RouterMessage message)))
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
              -> Router down downmess -> Router up upmess
connectRouter (RC f) r = ioRouter $ \oup iup ->
    do (idown,odown) <- forkRouter r
       (iboth, oboth) <- pipe
       forkIO $ forever $ readInput idown >>= (writeOutput oboth . Right)
       forkIO $ forever $ readInput iup >>= (writeOutput oboth . Left)
       f oup odown iboth
