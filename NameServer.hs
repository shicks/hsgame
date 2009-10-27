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
       forkIO $ forever $ readInput iii >>= (writeOutput busW . Right)
       forkIO $ forever $ readInput intoroomR >>= (writeOutput busW . Left)
       let handler xs =
               do m <- readInput busR
                  case m of
                    Left (Message x _ N) ->
                        do writeOutput fromroomW $ Message "server" x
                                (NamePrompt "Welcome to our chat server!")
                           handler xs
                    Left (Message _ _ (M (NamePrompt _))) ->
                        do putStrLn "silly client talks like a server!"
                           handler xs
                    Left (Message x _ (M (MyNameIs n))) ->
                        case lookup x xs of
                          Just f -> do putStrLn ("Your name is already "++f)
                                       handler xs
                          Nothing -> do putStrLn ("New user "++n)
                                        writeOutput ooo (Message n "server" N)
                                        handler $ (x,n):xs
                    Left (Message x t (M (NN z))) ->
                        case lookup x xs of
                        Nothing -> do putStrLn ("Message from ... "++x++
                                               " for "++t)
                                      handler xs -- log bad message?
                        Just nf ->
                          case lookup t xs of
                          Nothing ->
                             do putStrLn ("Message for "++t++"? from "++nf)
                                handler xs
                          Just nt ->
                               do putStrLn ("Message from "++nf)
                                  writeOutput ooo (Message nf nt (M z))
                                  handler xs
                    Right (Message fn tn z) ->
                        case rlookup fn xs of
                          Nothing -> do putStrLn ("foobar "++fn)
                                        handler xs
                          Just f ->
                              case rlookup tn xs of
                                Nothing -> do putStrLn ("bazbar "++tn)
                                              handler xs
                                Just t ->
                                   do writeOutput fromroomW (Message f t (NN z))
                                      handler xs
       forkIO $ handler [("server","server")]
       startRouter port fromroomR intoroomW 

rlookup :: Eq a => a -> [(b,a)] -> Maybe b
rlookup x y = lookup x $ map (\ (a,b) -> (b,a)) y
