module NameServer where

import TCP.Server ( startRouter, RouterMessage(..) )
import TCP.Chan ( ShowRead, Input, Output, writeOutput, readInput, pipe )
import TCP.Message ( Message(..) )

import Control.Concurrent ( forkIO )
import Control.Monad ( forever )

data Named message = NamePrompt String | MyNameIs String | NN message
                     deriving (Show, Read)
instance ShowRead message => ShowRead (Named message)

nameServer :: ShowRead message => Int
            -> Input (Message String message)
            -> Output (Message String (RouterMessage message))
            -> IO ()
nameServer port iii ooo =
    do (intoroomR,intoroomW) <- pipe
       (fromroomR,fromroomW) <- pipe
       (busR,busW) <- pipe
       let sendToClient y m = writeOutput fromroomW (Message "server" y m)
           handleNet xs =
               do m <- readInput intoroomR
                  case m of
                    Message x _ N ->
                        do sendToClient x
                                (NamePrompt "Welcome to our chat server!")
                           handleNet xs
                    Message _ _ (M (NamePrompt _)) ->
                        do putStrLn "silly client talks like a server!"
                           handleNet xs
                    Message x _ (M (MyNameIs n)) ->
                        case lookup x xs of
                          Just f -> do putStrLn ("Your name is already "++f)
                                       handleNet xs
                          Nothing -> do putStrLn ("New user "++n)
                                        writeOutput ooo (Message n "server" N)
                                        writeOutput busW (Left (n,x))
                                        handleNet $ (x,n):xs
                    Message x t (M (NN z)) ->
                           case lookup x xs of
                             Nothing -> do putStrLn ("Message from ... "++x)
                                           handleNet xs -- log bad message?
                             Just nf ->
                                 case lookup t xs of
                                   Nothing ->
                                       do putStrLn ("Message for unknown "++t++" from "++nf)
                                          handleNet xs
                                   Just nt ->
                                        do putStrLn ("Message from "++nf)
                                           writeOutput ooo (Message nf nt (M z))
                                           handleNet xs
       forkIO $ handleNet [("server","server")]
       forkIO $ forever $ readInput iii >>= (writeOutput busW . Right)
       let handleServer xs =
               do m <- readInput busR
                  case m of
                    Left x -> handleServer (x:xs)
                    Right (Message fn tn z) ->
                        case lookup fn xs of
                          Nothing -> handleServer xs
                          Just f ->
                              case lookup tn xs of
                                Nothing -> handleServer xs
                                Just t ->
                                   do writeOutput fromroomW (Message f t (NN z))
                                      handleServer xs
       forkIO $ handleServer [("server","server")]
       startRouter port fromroomR intoroomW 
