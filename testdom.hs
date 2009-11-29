import Dominion

import Control.Concurrent ( forkIO )
import TCP.Chan ( Input, Output, readInput, writeOutput, pipe )
import Control.Monad ( forever, when )
import Control.Monad.State ( evalStateT )
import Data.IORef ( newIORef, readIORef, writeIORef )
import System.IO ( hFlush, stdout )

import Debug.Trace ( trace )

mux :: String -> Input MessageToClient
    -> Output (String,MessageToClient) -> IO ()
mux name inc outc = forever $ readInput inc >>= writeOutput outc . (,) name

watch :: Input (String,MessageToClient) -> Output (QId,[Answer]) -> IO ()
watch inc outc =
              do last <- newIORef ""
                 forever $ do (n,q) <- readInput inc
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
                                    writeOutput outc (i,r)
    where spaces n = replicate ((40-length n)`div`2) ' '
          ints n [] = []
          ints n (s:ss) = case readsPrec 0 s of
                            [(x,"")] | x <= n -> x:ints n ss
                                     | otherwise -> trace ("rejecting "++show x) $ ints n ss
                            _ -> ints n ss
          xs !!! n = if n >= length xs then putStrLn ("!!!: "++show n++" >= length "++show xs) >> return (head xs)
                     else return $ xs !! n

main = do (c1i, c1o) <- pipe
          (c2i, c2o) <- pipe
          (cmuxi, cmuxo) <- pipe
          (cout_i, cout_o) <- pipe
          forkIO $ mux "Alice" c1i cmuxo
          forkIO $ mux "Bob" c2i cmuxo
          forkIO $ watch cmuxi cout_o
          state <- start [("Alice",c1o),("Bob",c2o)] cout_i
                   [secretChamber,chapel,cellar,village,remodel,
                    smithy,militia,thief,mine,market]
          evalStateT play state >>= print
