module Main where

import HTTP.Server ( httpServer, pure )
import HTTP.FileServer ( fileServer )
import HTTP.ChatServer ( chatServer, chatThread )
import TCP.Chan ( pipe )
import Control.Concurrent ( forkIO )

main :: IO ()
main = do (i,o) <- pipe
          forkIO $ chatThread i []
          httpServer 8081 $ pure $ chatServer o $ fileServer
