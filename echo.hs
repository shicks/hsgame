import TCP.Router ( pureRouter, RouterMessage(M) )
import TCP.Server ( startRouter )
import TCP.Message ( Message(Message) )

main :: IO ()
main = startRouter 12345 $ pureRouter echo
    where echo (Message f t (M s):ms) = Message t f (s :: String) : echo ms
          echo (_:ms) = echo ms
          echo [] = []
