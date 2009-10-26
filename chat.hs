import TCP.Server ( startRouter, RouterMessage(M,N) )
import TCP.Message ( Message(..) )
import TCP.Chan ( pipe, readInput, writeOutput )

import Control.Concurrent ( forkIO )

main :: IO ()
main = do let agentnames = map show [1 :: Int ..]
              makeagent = (++)
          (intoroomR,intoroomW) <- pipe
          (fromroomR,fromroomW) <- pipe
          let send x m y = writeOutput fromroomW (Message x y m)
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
          forkIO $ handleRoom []
          startRouter 12345 "server" makeagent agentnames fromroomR intoroomW
