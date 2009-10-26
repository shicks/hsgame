module TCP.Server ( startServer, startRouter, pureRouter,
                    RouterMessage(M,N) ) where

import Network ( withSocketsDo, listenOn, accept, Socket, PortID(PortNumber) )
import System.IO ( hSetBuffering, BufferMode(..) )
import Control.Concurrent ( forkIO )
import Control.Monad ( forever )

import TCP.Chan ( ShowRead, Input, Output, writeOutput, readInput, getInput,
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

data RouterMessage a = M a | N

startRouter :: (ShowRead message, Ord agent, Show agent) =>
               Int -> agent -> (String -> agent -> agent) -> [agent]
            -> Input (Message agent message)
            -> Output (Message agent (RouterMessage message))
            -> IO ()
startRouter port server nameagent agentnames iii ooo =
    withSocketsDo $
    do (server_i,server_o) <- pipe
       let serverThread agentmap =
               do m <- readInput server_i
                  case m of
                    Message a _ (Left o) ->
                        do writeOutput ooo (Message a server N)
                           serverThread ((a,o):agentmap)
                    Message f a (Right x)
                        | a == server -> do writeOutput ooo (Message f a (M x))
                                            serverThread agentmap
                    Message _ to (Right x) ->
                        case lookup to agentmap of
                          Just o -> do writeOutput o x
                                       serverThread agentmap
                          Nothing -> fail ("bad agent! "++show (to,x))
       forkIO $ serverThread []
       forkIO $ forever $ do Message fr to m <- readInput iii
                             putStrLn ("router got Message "++show (fr,to,m))
                             writeOutput server_o $ Message fr to (Right m)
       sock <- listenOn $ PortNumber $ fromIntegral port
       let listenForConnection (a:as) =
               do (h,n,_) <- accept sock -- ignoring the port number
                  hSetBuffering h LineBuffering
                  (i,o) <- handle2io h
                  let newagent = nameagent n a
                  writeOutput server_o $ Message newagent server (Left o)
                  forkIO $ forever $
                         do x <- readInput i
                            writeOutput server_o
                                       (Message newagent server (Right x))
                  listenForConnection as
           listenForConnection [] = fail "not enough possible agent names!"
       listenForConnection agentnames

pureRouter :: (ShowRead message, Ord agent, Show agent) =>
               Int -> agent -> (String -> agent -> agent) -> [agent]
            -> ([Message agent (RouterMessage message)]
                    -> [Message agent message])
            -> IO ()
pureRouter port server nameagent agentnames f =
    do (i,o) <- pipe
       (i2,o2) <- pipe
       forkIO $ do x <- getInput i
                   mapM_ (writeOutput o2) $ f x
       startRouter port server nameagent agentnames i2 o
