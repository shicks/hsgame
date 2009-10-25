import TCP.Server ( startRouter )
import TCP.Message ( Message(..) )

main :: IO ()
main = do let agentnames = map show [1 :: Int ..]
              makeagent = (++)
          startRouter 12345 "server" makeagent agentnames $
                          \sendMessage agents (Message from _ chat) ->
              do let send to = sendMessage (Message from to
                                                        (from++" says: "++chat))
                 mapM_ send $ filter (/= from) agents
