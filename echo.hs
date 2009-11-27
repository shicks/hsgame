import TCP.ServerTypes ( pureServer, ServerMessage(M) )
import TCP.Server ( runServerTCP )
import TCP.Message ( Message(Message) )

main :: IO ()
main = runServerTCP 12345 $ pureServer echo
    where echo (Message f t (M s):ms) = Message t f (s :: String) : echo ms
          echo (_:ms) = echo ms
          echo [] = []
