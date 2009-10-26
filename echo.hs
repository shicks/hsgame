import TCP.Server ( pureRouter, RouterMessage(M) )
import TCP.Message ( Message(Message) )

main :: IO ()
main = pureRouter 12345 "server" (++) agentnames echo
    where agentnames = map show [1 :: Int ..]
          echo (Message f t (M s):ms) = Message t f (s :: String) : echo ms
          echo (_:ms) = echo ms
          echo [] = []
