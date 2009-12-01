module Main ( main ) where

import System.Environment ( getArgs )
import Control.Monad ( forever )
import Control.Concurrent ( forkIO )

import TCP.ServerTypes ( ServerMessage(M,N), ioServer, modifyServer )
import TCP.Server ( runServerTCP )
import TCP.Client ( runClientTCP, ioClient )
import TCP.Message ( Message(..) )
import TCP.Chan ( readInput, writeOutput )
import NamePicker ( pickNames, namedClient )
import Lobby ( lobby, lobbyClient )

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> serve
            [host] -> client host
            _ -> fail "need zero or one argument (a host)"

client :: String -> IO ()
client host = runClientTCP host 12345 $ namedClient $
              lobbyClient "tablename is String" $ ioClient $ \i o ->
              do x <- readInput i
                 putStrLn x
                 forkIO $ forever $ getLine >>= writeOutput o
                 forever $ readInput i >>= putStrLn              

serve :: IO ()
serve =
    runServerTCP 12345 $ modifyServer (pickNames "server" "server") $
                 lobby "tabletype is String" $ ioServer $
       \fromroomW intoroomR ->
          do let send x m y = writeOutput fromroomW (Message x y m)
                 handleRoom xs =
                  do m <- readInput intoroomR
                     case m of
                       Message x _ N ->
                           do send "server" ("Welcome, "++x++"!") x
                              mapM_ (send "server"
                                     ("Welcome "++x++" everyone!")) xs
                              handleRoom (x:xs)
                       Message f t (M s) ->
                           if t == "server"
                           then do mapM_ (send f (f++" says: "++s))
                                             (filter (/= f) $ xs)
                                   handleRoom xs
                           else do send f (f++" privately says: "++s) t
                                   handleRoom xs
             handleRoom []
