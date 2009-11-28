import TCP.Router ( RouterMessage(M,N), ioRouter, connectRouter )
import TCP.Server ( startRouter )
import TCP.Message ( Message(..) )
import TCP.Chan ( readInput, writeOutput )
import NameServer ( nameServer )

main :: IO ()
main = startRouter 12345 $
       connectRouter (nameServer "server" "server") $ ioRouter $
       \fromroomW intoroomR ->
          do let send x m y = writeOutput fromroomW (Message x y m)
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
             handleRoom []
