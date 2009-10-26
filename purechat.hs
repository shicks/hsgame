import TCP.Server ( pureRouter, RouterMessage(M,N) )
import TCP.Message ( Message(..) )

chat :: [(String,String)] -> [Message String (RouterMessage String)]
     -> [Message String String]
chat xs (Message x _ N:ms) =
    Message "server" x "Welcome to our chat server!" :
    Message "server" x "What is your name?" : chat xs ms
chat xs (Message x t (M s):ms) =
    case lookup x xs of
      Nothing ->
          map (\ (y,_) -> Message "server" y
                          ("Welcome "++s++", also known as "++x)) xs
                                            ++ chat ((x,s):xs) ms
      Just f -> if t == "server"
                then map (\ (y,_) -> Message x y (f++" says: "++s)) xs
                     ++ chat xs ms
                else Message x t (f++" privately says: "++s) : chat xs ms
chat _ [] = []

main :: IO ()
main = pureRouter 12345 "server" makeagent agentnames $ chat []
    where agentnames = map show [1 :: Int ..]
          makeagent = (++)
