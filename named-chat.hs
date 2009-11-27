import TCP.ServerTypes ( ServerMessage(M,N), ioServer, modifyServer )
import TCP.Server ( runServerTCP )
import TCP.Message ( Message(..) )
import TCP.Chan ( readInput, writeOutput )
import NamePicker ( pickNames )

main :: IO ()
main = runServerTCP 12345 $
       modifyServer (pickNames "server" "server") $ ioServer $
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
