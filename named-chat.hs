import TCP.Server ( RouterMessage(M,N) )
import TCP.Message ( Message(..) )
import TCP.Chan ( pipe, readInput, writeOutput )
import NameServer ( nameServer )

import Control.Concurrent ( forkIO )

main :: IO ()
main = do (intoroomR,intoroomW) <- pipe
          (fromroomR,fromroomW) <- pipe
          let send x m y = writeOutput fromroomW (Message x y m)
              handleRoom xs =
                  do m <- readInput intoroomR
                     case m of
                       Message x _ N ->
                           do send "server" ("Welcome, "++x) x
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
          forkIO $ handleRoom []
          nameServer 12345 "server" fromroomR intoroomW
