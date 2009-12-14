module Main where

import HTTP.Server ( httpServer, pure )
import HTTP.FileServer ( fileServer )
import HTTP.ChatServer ( chatHandler )
import HTTP.LoginServer ( LoginMessage(SendMessage),
                          loginServer, loginThread, dirServer )
import HTTP.Handlers ( useHandler )
-- import Dominion.Server ( dominionServer, dominionThread )
import TCP.Chan ( pipe, writeOutput )
import Control.Concurrent ( forkIO )

main :: IO ()
main = do (li,lo) <- pipe -- we could make servers an IO (.. -> IO Response)
          forkIO $ loginThread li []
          let sendM a s = writeOutput lo $ SendMessage a s
          chat <- chatHandler "chat" sendM >>= useHandler
          silly <- chatHandler "silly" sendM >>= useHandler
          -- forkIO $ dominionThread ddi []
          httpServer 8081 $ pure $ loginServer lo fileServer $
                     dirServer [(Just "chat",chat),
                                (Just "silly", silly)]
                                --(Just "dominion",dominionServer dso)]
