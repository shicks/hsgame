module TCP.Router ( Router, RouterMessage(..), forkRouter,
                    ioRouter, pureRouter ) where

import TCP.Chan ( Input, Output, pipe, getInput, writeOutput )
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
