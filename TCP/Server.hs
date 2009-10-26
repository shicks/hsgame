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

startRouter :: ShowRead message => Int
            -> Input (Message String message)
            -> Output (Message String (RouterMessage message))
            -> IO ()
startRouter port iii ooo =
    withSocketsDo $
    do (server_i,server_o) <- pipe
       let serverThread agentmap =
               do m <- readInput server_i
                  case m of
                    Message a _ (Left (nameo,o)) ->
                        do let rename x = if x `notElem` map fst agentmap
                                          then x
                                          else rename (x++"'")
                               aa = rename a
                           writeOutput nameo aa -- report name to socket thread
                           writeOutput ooo (Message aa "server" N)
                           serverThread ((aa,o):agentmap)
                    Message f "server" (Right x) ->
                        do writeOutput ooo (Message f "server" (M x))
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
       forever $ do (h,n,_) <- accept sock -- ignoring the port number
                    hSetBuffering h LineBuffering
                    (i,o) <- handle2io h
                    (namei, nameo) <- pipe
                    writeOutput server_o $ Message n "server" (Left (nameo,o))
                    name <- readInput namei
                    forkIO $ forever $
                          do x <- readInput i
                             writeOutput server_o
                                             (Message name "server" (Right x))

pureRouter :: ShowRead message => Int
           -> ([Message String (RouterMessage message)]
                   -> [Message String message])
           -> IO ()
pureRouter port f =
    do (i,o) <- pipe
       (i2,o2) <- pipe
       forkIO $ do x <- getInput i
                   mapM_ (writeOutput o2) $ f x
       startRouter port i2 o
