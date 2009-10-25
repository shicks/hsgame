module TCP.Chan ( ShowRead(..), handle2chans ) where

import Control.Monad ( forever )
import System.IO ( Handle, hPutStrLn, hGetLine )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( Chan, newChan, writeChan, readChan )

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
       forkIO $ forever (do x <- hGetLine h
                            case readLine $ init x of
                              Just a -> writeChan i a
                              Nothing -> fail ("bad data: "++x))
                  `catch` (\_ -> return ())
       forkIO $ forever $ do x <- readChan o
                             hPutStrLn h $ showLine x
       return (i,o)

