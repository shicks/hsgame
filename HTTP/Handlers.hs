{-# LANGUAGE ExistentialQuantification #-}

module HTTP.Handlers ( useHandler, Message(..), Handler(..) ) where

import HTTP.LoginServer ( Agent )
import HTTP.Response ( Response )
import TCP.Chan ( Output, readInput, writeOutput, pipe )

import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar ( newEmptyMVar, putMVar, takeMVar )

-- This is a new framework for sending messages...

 -- Message is not quite a state monad...
data Message st = Message { runJob :: st -> IO (st, Response) }

startServerThread :: st -> IO (Output (Message st))
startServerThread initial = do (i,o) <- pipe
                               let handle st = do m <- readInput i
                                                  (st',_) <- runJob m st
                                                  handle st'
                               forkIO $ handle initial
                               return o

data Handler =
    forall st. Handler st (Agent -> [String] -> [(String,String)] -> Message st)

useHandler :: Handler
              -> IO (Agent -> [String] -> [(String,String)] -> IO Response)
useHandler (Handler initial handler) =
    do o <- startServerThread initial
       let h a b c = do mv <- newEmptyMVar
                        writeOutput o $ Message $ \st ->
                            do (st',r) <- runJob (handler a b c) st
                               putMVar mv r
                               return (st',r)
                        takeMVar mv
       return h
