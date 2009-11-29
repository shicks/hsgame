import Dominion

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( Chan, readChan, writeChan, newChan )
import Control.Monad ( forever, when )
import Control.Monad.State ( evalStateT )
import Data.IORef ( newIORef, readIORef, writeIORef )
import System.IO ( hFlush, stdout )

import Debug.Trace ( trace )

mux :: String -> Chan MessageToClient -> Chan (String,MessageToClient) -> IO ()
mux name inc outc = forever $ readChan inc >>= writeChan outc . (,) name

watch :: Chan (String,MessageToClient) -> Chan (QId,[Answer]) -> IO ()
watch inc outc =
              do last <- newIORef ""
                 forever $ do (n,q) <- readChan inc
                              lastR <- readIORef last
                              when (n/=lastR) $ do
                                putStrLn "\n\n\n"
                                putStrLn $ replicate 40 '='
                                putStrLn $ spaces n++n++spaces n
                                putStrLn $ replicate 40 '-' ++ "\n"
                                writeIORef last n
                              case q of
                                Info m -> case m of
                                            InfoMessage s -> putStrLn s
                                            -- _ -> putStrLn $ show m
                                Question i m as (a0,a1) -> do
                                    putStrLn $ "Question: " ++ show m
                                    putStrLn "Options:"
                                    mapM_ (\(n,a) -> putStrLn $ "  " ++ show n
                                                     ++ ": " ++ show a) $
                                          zip [1..] as
                                    putStr $ "Enter " ++ show a0 ++ " to "
                                               ++ show a1 ++
                                               " numbers, separated by spaces: "
                                    hFlush stdout
                                    ans <- (ints (length as) . words)
                                           `fmap` getLine
                                    r <- mapM (\n -> as!!!(n-1)) ans
                                    writeChan outc (i,r)
    where spaces n = replicate ((40-length n)`div`2) ' '
          ints n [] = []
          ints n (s:ss) = case readsPrec 0 s of
                            [(x,"")] | x <= n -> x:ints n ss
                                     | otherwise -> trace ("rejecting "++show x) $ ints n ss
                            _ -> ints n ss
          xs !!! n = if n >= length xs then putStrLn ("!!!: "++show n++" >= length "++show xs) >> return (head xs)
                     else return $ xs !! n

main = do c1 <- newChan
          c2 <- newChan
          cmux <- newChan
          cout <- newChan
          forkIO $ mux "Alice" c1 cmux
          forkIO $ mux "Bob" c2 cmux
          forkIO $ watch cmux cout
          state <- start [("Alice",c1),("Bob",c2)] cout
                   [secretChamber,chapel,cellar,village,remodel,
                    smithy,militia,thief,mine,market]
          evalStateT play state >>= print
