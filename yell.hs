import TCP.Server ( runServerTCP )
import TCP.ServerTypes ( ioServer, ServerMessage(M,N) )
import TCP.Message ( Message(Message) )
import TCP.Chan ( writeOutput, readInput )

import Control.Monad ( forever )
import Control.Concurrent ( forkIO )

main :: IO ()
main = runServerTCP 12345 $  ioServer handle
    where handle fromRoomW intoRoomR = forever $
              do m <- readInput intoRoomR
                 case m of
                   Message x _ N ->
                       do forkIO $ forever $
                                 writeOutput fromRoomW (Message "server" x $
                                                                    "hello "++x)
                          return ()
                   Message x _ (M s) ->
                       writeOutput fromRoomW (Message "server" x $
                                              "I don't care about "++s)

