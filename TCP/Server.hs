module TCP.Server ( startServer, startRouter ) where

import Network ( withSocketsDo, listenOn, accept, Socket, PortID(PortNumber) )
import System.IO ( hSetBuffering, BufferMode(..) )
import Control.Concurrent ( forkIO )
import Control.Monad ( forever )

import TCP.Chan ( ShowRead, Input, Output, writeOutput, readInput,
                  pipe, handle2io )
import TCP.Message ( Message(..) )

listenForClients :: ShowRead a =>
                    Socket -> (String -> Input a -> Output a -> IO ()) -> IO ()
listenForClients sock job =
    forever $ do (h,n,_) <- accept sock  -- ignoring the port number
                 hSetBuffering h LineBuffering
                 (i,o) <- handle2io h
                 forkIO $ job n i o

startServer :: ShowRead a =>
               Int -> (String -> Input a -> Output a -> IO ()) -> IO ()
startServer p job =
    withSocketsDo $ do s <- listenOn $ PortNumber $ fromIntegral p
                       listenForClients s job

data ClientM agent message = ClientM (Message agent message)
                           | NewClient agent (Output message)

startRouter :: (ShowRead message, Ord agent) =>
               Int -> agent -> (String -> agent -> agent) -> [agent]
            -> ((Message agent message -> IO ()) -> [agent]
                    -> Message agent message -> IO ()) -> IO ()
startRouter port server nameagent agentnames handleMessage =
    withSocketsDo $
    do (server_i,server_o) <- pipe
       let sendMessage = writeOutput server_o . ClientM
           serverThread agentmap =
               do m <- readInput server_i
                  case m of
                    NewClient a o -> serverThread ((a,o):agentmap)
                    ClientM mess@(Message _ a _)
                        | a == server ->
                            do handleMessage sendMessage (map fst agentmap) mess
                               serverThread agentmap
                    ClientM (Message _ to x) ->
                        case lookup to agentmap of
                          Just o -> do writeOutput o x
                                       serverThread agentmap
                          Nothing -> fail "bad agent!"
       forkIO $ serverThread []
       sock <- listenOn $ PortNumber $ fromIntegral port
       let listenForConnection (a:as) =
               do (h,n,_) <- accept sock -- ignoring the port number
                  hSetBuffering h LineBuffering
                  (i,o) <- handle2io h
                  let newagent = nameagent n a
                  writeOutput server_o $ NewClient newagent o
                  forkIO $ forever $
                         do x <- readInput i
                            sendMessage (Message newagent server x)
                  listenForConnection as
           listenForConnection [] = fail "not enough possible agent names!"
       listenForConnection agentnames
