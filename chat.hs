||| add lobby-chat which supports tables. >>>
module Main ( main ) where

import Control.Monad ( forever )
import System.Environment ( getArgs )

import TCP.ServerTypes ( ServerMessage(M,N), ioServer )
import TCP.Client ( runClientTCP, decoupledClient )
import TCP.Server ( runServerTCP )
import TCP.Message ( Message(..) )
import TCP.Chan ( readInput, writeOutput )

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> serve
            [host] -> client host
            _ -> fail "need zero or one argument (a host)"

client :: String -> IO ()
client host = runClientTCP host 12345 $ decoupledClient reader writer
    where reader i = forever $ readInput i >>= putStrLn              
          writer o = forever $ getLine >>= writeOutput o

serve :: IO ()
serve = runServerTCP 12345 $ ioServer $ \fromroomW intoroomR ->
       do let send x m y = writeOutput fromroomW (Message x y m)
              handleRoom xs =
                  do m <- readInput intoroomR
                     case m of
                       Message x _ N ->
                           do send "server" "Welcome to our chat server!" x
                              send "server" "What is your name?" x
                              handleRoom xs
                       Message x t (M s) ->
                           case lookup x xs of
                             Nothing ->  do mapM_ (send "server"
                                                   ("Welcome "++s++
                                                    ", also known as "++x))
                                                      $ map fst xs
                                            handleRoom ((x,s):xs)
                             Just f ->
                                 if t == "server"
                                 then do mapM_ (send x (f++" says: "++s))
                                                   (filter (/= x) $ map fst xs)
                                         handleRoom xs
                                 else do send x (f++" privately says: "++s) t
                                         handleRoom xs
          handleRoom []


<<< add lobby-chat which supports tables. |||
||| reorganize actions in Game.hs >>>
import TCP.Server ( startServer )
import Control.Monad ( forever )
import System.IO ( hPutStrLn, hGetLine )

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( newChan, writeChan, readChan, dupChan )

main = do c <- newChan
          forkIO $ forever $ getLine >>= writeChan c
          forkIO $ forever $ readChan c >>= putStrLn
          startServer 12345 $ \n h -> do c' <- dupChan c
                                         forkIO $ forever $ do l <- readChan c'
                                                               hPutStrLn h l
                                         forever $ do l <- hGetLine h
                                                      hPutStrLn h l
                                                      writeChan c' l

<<< reorganize actions in Game.hs |||
