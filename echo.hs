||| add lobby-chat which supports tables. >>>
import TCP.ServerTypes ( pureServer, ServerMessage(M) )
import TCP.Server ( runServerTCP )
import TCP.Message ( Message(Message) )

main :: IO ()
main = runServerTCP 12345 $ pureServer echo
    where echo (Message f t (M s):ms) = Message t f (s :: String) : echo ms
          echo (_:ms) = echo ms
          echo [] = []

<<< add lobby-chat which supports tables. |||
||| reorganize actions in Game.hs >>>
import TCP.Server ( startServer )
import Control.Monad ( forever )
import System.IO ( hPutStrLn, hGetLine )

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( newChan, writeChan, readChan, dupChan )

main = do c <- newChan
          forkIO $ forever $ getLine >>= writeChan c
          startServer 12345 $ \n h -> do c' <- dupChan c
                                         forkIO $ readChan c >>= hPutStrLn h
                                         forever $ do l <- hGetLine h
                                                      hPutStrLn h l
                                                      putStrLn $ n ++ ": " ++ l

<<< reorganize actions in Game.hs |||
