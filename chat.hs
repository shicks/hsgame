import TCP.Server ( startServer )

import Control.Monad ( forever )
import System.IO ( Handle, hPutStrLn, hGetLine, stdout )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( Chan, newChan, writeChan, readChan, dupChan )

class (Show a, Read a) => ShowRead a where
    showLine :: a -> String
    showLine = show
    readLine :: String -> Maybe a
    readLine x = do [(a,"")] <- Just $ reads x
                    Just a
class (Read c, Show c) => CharLike c where
    toChar :: c -> Char
    fromChar :: Char -> c
instance CharLike Char where
    toChar = id
    fromChar = id
instance CharLike c => ShowRead [c] where
    showLine = map toChar
    readLine = Just . map fromChar

handle2chans :: (ShowRead i, ShowRead o) => Handle -> IO (Chan i, Chan o)
handle2chans h =
    do i <- newChan
       o <- newChan
       forkIO $ forever $ do x <- hGetLine h
                             case readLine x of
                               Just a -> writeChan i a
                               Nothing -> fail ("bad data: "++x)
       forkIO $ forever $ do x <- readChan o
                             hPutStrLn h $ showLine x
       return (i,o)

heSaysC :: Chan String -> String -> (String, String) -> IO ()
heSaysC h me (him, statement)
       | him == me = return ()
       | otherwise = writeChan h (him++" says: "++statement)

heSays :: Handle -> String -> (String, String) -> IO ()
heSays h me (him, statement)
       | him == me = return ()
       | otherwise = hPutStrLn h (him++" says: "++statement)

main :: IO ()
main = do c <- newChan
          forkIO $ forever $ do l <- getLine
                                writeChan c ("server",l)
          forkIO $ forever $ readChan c >>= heSays stdout "server"
          {-
          startServer 12345 $ \n h -> do c' <- dupChan c
                                         writeChan c' ("server",
                                                       "Everyone, welcome "++n)
                                         forkIO $ forever $
                                                readChan c' >>= heSays h n
                                         forever $ do l <- hGetLine h
                                                      writeChan c' (n,l)
          -}
          startServer 12345 $ \n h ->
              do (i,o) <- handle2chans h
                 c' <- dupChan c
                 writeChan c' ("server", "Everyone, welcome "++n)
                 forkIO $ forever $ readChan c' >>= heSaysC o n
                 forever $ do l <- readChan i
                              writeChan c' (n,l)
