import Dominion

import TCP.Client ( runClientTCP, ioClient )
import Control.Concurrent ( forkIO )
import TCP.Chan ( Input, Output, readInput, writeOutput, pipe )
import Control.Monad ( forever, when )
import Control.Monad.State ( evalStateT )
import Data.IORef ( newIORef, readIORef, writeIORef )
import System.IO ( hFlush, stdout )
import System.Environment ( getArgs )

import Debug.Trace ( trace )

client :: String -> Input MessageToClient -> Output ResponseFromClient -> IO ()
client n inc outc = handle prefix
    where handle msg = do q <- readInput inc
                          case q of
                            Info m -> case m of
                                        InfoMessage s -> handle (msg++s++"\n")
                                        -- _ -> putStrLn $ show m
                            Question i m as (a0,a1) ->
                                do putStrLn msg
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
                                   writeOutput outc $ ResponseFromClient i r
                                   handle prefix
          spaces = replicate ((40-length n)`div`2) ' '
          ints n [] = []
          ints n (s:ss) = case readsPrec 0 s of
                            [(x,"")] | x <= n -> x:ints n ss
                                     | otherwise -> trace ("rejecting "++show x) $ ints n ss
                            _ -> ints n ss
          xs !!! n = if n >= length xs then putStrLn ("!!!: "++show n++" >= length "++show xs) >> return (head xs)
                     else return $ xs !! n
          prefix = unlines["",replicate 40 '=',
                           spaces++n++spaces,
                           replicate 40 '-',""]

main :: IO ()
main =
    do args <- getArgs
       case args of
         [name,hostname] ->
             runClientTCP hostname 12345 $ ioClient $ client name
         _ -> do
          (c1i, c1o) <- pipe
          (c2i, c2o) <- pipe
          (cout_i, cout_o) <- pipe
          forkIO $ client "Alice" c1i cout_o
          forkIO $ client "Bob" c2i cout_o
          state <- start [("Alice",c1o),("Bob",c2o)] cout_i
                   [secretChamber,chapel,cellar,village,remodel,
                    smithy,militia,thief,mine,market]
          evalStateT play state >>= print
