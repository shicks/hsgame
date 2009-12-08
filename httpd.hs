module Main where

import HTTP.Server ( httpServer, pure )
import HTTP.FileServer ( fileServer )
import HTTP.ChatServer ( chatServer, chatThread )
import HTTP.LoginServer ( loginServer, loginThread, dirServer )
-- import Dominion.Server ( dominionServer, dominionThread )
import TCP.Chan ( pipe )
import Control.Concurrent ( forkIO )

main :: IO ()
main = do (li,lo) <- pipe -- we could make servers an IO (.. -> IO Response)
          (ci,co) <- pipe -- and then all the pipes and forkIOs would be swept
          -- (ddi,ddo) <- pipe -- under the rug...
          forkIO $ loginThread li []
          forkIO $ chatThread ci lo []
          -- forkIO $ dominionThread ddi []
          httpServer 8081 $ pure $ loginServer lo fileServer $
                     dirServer [(Just "chat",chatServer co)]
                                --(Just "dominion",dominionServer dso)]
