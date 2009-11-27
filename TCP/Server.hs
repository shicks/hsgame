module TCP.Server ( startServer, startRouter ) where

import Network ( withSocketsDo, listenOn, accept, Socket, PortID(PortNumber) )
import System.IO ( hSetBuffering, BufferMode(..) )
import Control.Concurrent ( forkIO )
import Control.Monad ( forever )

import TCP.Chan ( ShowRead, Input, Output, writeOutput, readInput, pipe,
                  handle2io )
import TCP.Message ( Message(..) )
import TCP.Router ( Router, RouterMessage(..), forkRouter )

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

data Internal name toclient toserver = ToClient toclient
                                     | ToServer toserver
                                     | NewClient (Output toclient) (Output name)

startRouter :: (ShowRead toclient, ShowRead toserver) =>
               Int -> Router String toclient (RouterMessage toserver) -> IO ()
startRouter port r =
    withSocketsDo $
    do (iii, ooo) <- forkRouter r
       (server_i,server_o) <- pipe
       let serverThread agentmap =
               do m <- readInput server_i
                  case m of
                    Message a _ (NewClient o nameo) ->
                        do let rename x = if x `notElem` map fst agentmap
                                          then x
                                          else rename (x++"'")
                               aa = rename a
                           writeOutput nameo aa -- report name to socket thread
                           writeOutput ooo (Message aa "server" N)
                           serverThread ((aa,o):agentmap)
                    Message f _ (ToServer x) ->
                        do writeOutput ooo (Message f "server" (M x))
                           serverThread agentmap
                    Message _ to (ToClient x) ->
                        case lookup to agentmap of
                          Just o -> do writeOutput o x
                                       serverThread agentmap
                          Nothing -> fail ("bad agent! "++show (to,x))
       forkIO $ serverThread []
       forkIO $ forever $ do Message fr to m <- readInput iii
                             putStrLn ("router got Message "++show (fr,to,m))
                             writeOutput server_o $ Message fr to (ToClient m)
       sock <- listenOn $ PortNumber $ fromIntegral port
       forever $ do (h,n,_) <- accept sock -- ignoring the port number
                    hSetBuffering h LineBuffering
                    (i,o) <- handle2io h
                    (namei, nameo) <- pipe
                    writeOutput server_o $
                                Message n "server" (NewClient o nameo)
                    name <- readInput namei
                    forkIO $ forever $
                          do x <- readInput i
                             writeOutput server_o $
                                         Message name "server" (ToServer x)
