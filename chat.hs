import TCP.Server ( startRouter )
import TCP.Message ( Message(..) )
import TCP.Chan ( readInput, Output, writeOutput, handle2o )

import Control.Monad ( forever )
import System.IO ( stdout )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( newChan, writeChan, readChan, dupChan )

heSaysC :: Output String -> String -> (String, String) -> IO ()
heSaysC h me (him, statement)
       | him == me = return ()
       | otherwise = writeOutput h (him++" says: "++statement)

main :: IO ()
main = do c <- newChan
          oo <- handle2o stdout
          forkIO $ forever $ do l <- getLine
                                writeChan c ("server",l)
          forkIO $ forever $ readChan c >>= heSaysC oo "server"
          let agentnames = map show [1..]
              makeagent = (++)
          startRouter 12345 "server" makeagent agentnames $
                          \sendMessage agents (Message from _ chat) ->
              do let send to = sendMessage (Message from to
                                                        (from++" says: "++chat))
                 mapM_ send $ filter (/= from) agents
