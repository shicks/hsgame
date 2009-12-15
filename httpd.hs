module Main where

import HTTP.Server ( httpServer, pure )
import HTTP.FileServer ( fileServer )
import HTTP.ChatServer ( chatHandler )
import HTTP.LoginServer ( LoginMessage(SendMessage), loginServer, loginThread )
import HTTP.DirHandler ( dirHandler )

import TCP.Chan ( pipe, writeOutput )
import Control.Concurrent ( forkIO )

main :: IO ()
main = do (li,lo) <- pipe
          forkIO $ loginThread li []
          let sendM a s = writeOutput lo $ SendMessage a s
          dh <- dirHandler [(Just "chat", chatHandler "chat" sendM),
                            (Just "silly", chatHandler "silly" sendM)]
          httpServer 8081 $ pure $ loginServer lo fileServer dh
