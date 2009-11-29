||| Merge >>>
module TCP.Chan ( ShowRead(..), handle2io, handle2i, handle2o,
                  pipe, interacting,
                  Input, readInput, getInput, Output, writeOutput ) where

import Control.Monad ( forever )
import System.IO ( Handle, hPutStrLn, hGetLine )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( Chan, newChan, writeChan, readChan,
                                 getChanContents )

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

newtype Input a = Input (Chan a)
newtype Output a = Output (Chan a)

readInput :: Input a -> IO a
readInput (Input c) = readChan c

writeOutput :: Output a -> a -> IO ()
writeOutput (Output c) = writeChan c

handle2io :: (ShowRead i, ShowRead o) => Handle -> IO (Input i, Output o)
handle2io h = do i <- handle2i h
                 o <- handle2o h
                 return (i, o)

handle2o :: ShowRead o => Handle -> IO (Output o)
handle2o h = do o <- newChan
                forkIO $ forever $ do x <- readChan o
                                      hPutStrLn h $ showLine x
                return (Output o)

handle2i :: ShowRead i => Handle -> IO (Input i)
handle2i h = do i <- newChan
                forkIO $ forever (do x <- hGetLine h
                                     case readLine $ init x of
                                       Just a -> writeChan i a
                                       Nothing -> fail ("bad data: "++x))
                           `catch` (\_ -> return ())
                return (Input i)

pipe :: IO (Input a, Output a)
pipe = do c <- newChan
          return (Input c, Output c)

interacting :: Input i -> Output o -> ([i] -> [o]) -> IO ()
interacting (Input i) o f =
    do is <- getChanContents i
       forkIO $ mapM_ (writeOutput o) $ f is
       return ()

getInput :: Input a -> IO [a] -- lazy
getInput (Input i) = getChanContents i

<<< Merge |||
||| Merge stupidly? >>>
module TCP.Chan ( ShowRead(..), handle2io, handle2i, handle2o,
                  pipe, interacting,
                  Input, readInput, getInput, Output, writeOutput ) where

import Control.Monad ( forever )
import System.IO ( Handle, hPutStrLn, hGetLine )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( Chan, newChan, writeChan, readChan,
                                 getChanContents )

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

newtype Input a = Input (Chan a)
newtype Output a = Output (Chan a)

readInput :: Input a -> IO a
readInput (Input c) = readChan c

writeOutput :: Output a -> a -> IO ()
writeOutput (Output c) = writeChan c

handle2io :: (ShowRead i, ShowRead o) => Handle -> IO (Input i, Output o)
handle2io h = do i <- handle2i h
                 o <- handle2o h
                 return (i, o)

handle2o :: ShowRead o => Handle -> IO (Output o)
handle2o h = do o <- newChan
                forkIO $ forever $ do x <- readChan o
                                      hPutStrLn h $ showLine x
                return (Output o)

handle2i :: ShowRead i => Handle -> IO (Input i)
handle2i h = do i <- newChan
                forkIO $ forever (do x <- hGetLine h
                                     case readLine x of
                                       Just a -> writeChan i a
                                       Nothing -> fail ("bad data: "++x))
                           `catch` (\_ -> return ())
                return (Input i)

pipe :: IO (Input a, Output a)
pipe = do c <- newChan
          return (Input c, Output c)

interacting :: Input i -> Output o -> ([i] -> [o]) -> IO ()
interacting (Input i) o f =
    do is <- getChanContents i
       forkIO $ mapM_ (writeOutput o) $ f is
       return ()

getInput :: Input a -> IO [a] -- lazy
getInput (Input i) = getChanContents i

<<< Merge stupidly? |||
