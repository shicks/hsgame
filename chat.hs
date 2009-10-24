import TCP.Server ( startServer )
import YAML ( YAML, fromNode, parseYaml, showYaml )

import Control.Monad ( forever )
import System.IO ( Handle, hPutStrLn, hPutStr, hGetLine, hGetContents, stdout )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( Chan, newChan, writeChan, readChan, dupChan )

yamlHandle :: (YAML i, YAML o) => Handle -> IO (Chan i, Chan o)
yamlHandle h =
    do i <- newChan
       o <- newChan
       forkIO $ do nodes <- parseYaml `fmap` hGetContents h
                   let writeC n = case fromNode n of
                                    Just x -> do putStrLn "got one"
                                                 writeChan i x
                                    Nothing -> do putStrLn "oopps"
                                                  fail ("bad node "++show n)
                   mapM_ writeC nodes
       forkIO $ forever $ do x <- readChan o
                             hPutStr h $ showYaml [x]
       return (i,o)

{-
heSays :: Chan String -> String -> (String, String) -> IO ()
heSays h me (him, statement)
       | him == me = return ()
       | otherwise = writeChan h (him++" says: "++statement)
-}

heSays :: Handle -> String -> (String, String) -> IO ()
heSays h me (him, statement)
       | him == me = return ()
       | otherwise = hPutStrLn h (him++" says: "++statement)

main :: IO ()
main = do c <- newChan
          forkIO $ forever $ do l <- getLine
                                writeChan c ("server",l)
          forkIO $ forever $ readChan c >>= heSays stdout "server"
          startServer 12345 $ \n h -> do c' <- dupChan c
                                         writeChan c' ("server",
                                                       "Everyone, welcome "++n)
                                         forkIO $ forever $
                                                readChan c' >>= heSays h n
                                         forever $ do l <- hGetLine h
                                                      writeChan c' (n,l)

         {-
          startServer 12345 $ \n h ->
              do (i,o) <- yamlHandle h
                 c' <- dupChan c
                 writeChan c' ("server",
                               "Everyone, welcome "++n)
                 forkIO $ forever $ readChan c' >>= heSays o n
                 forever $ do l <- readChan i
                              writeChan c' (n,l)
-}
