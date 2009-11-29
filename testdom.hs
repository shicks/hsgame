import Dominion

import TCP.Message ( Message(..) )
import TCP.Client ( runClientTCP, ioClient )
import TCP.Server ( runServerTCP )
import TCP.ServerTypes ( ServerMessage(..), ioServer )

import Control.Concurrent ( forkIO )
import TCP.Chan ( Input, Output, readInput, writeOutput, pipe )
import Control.Monad ( forever, when )
import Control.Monad.State ( evalStateT )
import Data.IORef ( newIORef, readIORef, writeIORef )
import System.IO ( hFlush, stdout )
import System.Environment ( getArgs )
import System.Random ( randomRIO )

import Debug.Trace ( trace )

client :: String -> Input MessageToClient -> Output ResponseFromClient -> IO ()
client n inc outc = handle prefix
    where handle msg = do putStrLn "waiting on input from server..."
                          q <- readInput inc
                          putStrLn "got input from server."
                          putStrLn $ show q
                          case q of
                            Info m -> case m of
                                        InfoMessage s -> handle (msg++s++"\n")
                                        -- _ -> putStrLn $ show m
                            Question i m as (a0,a1) ->
                                do putStrLn msg
                                   putStrLn $ "Question: " ++ show m
                                   putStrLn "Options:"
                                   let pretty (Choose s) = s
                                       pretty (PickCard c) = cname c++
                                           " ("++show (cprice c)++") ["++
                                               show (cid c)++"]"
                                   mapM_ (\(n,a) -> putStrLn $ "  " ++ show n
                                                    ++ ": " ++ pretty a) $
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

bot :: String -> Input MessageToClient -> Output ResponseFromClient -> IO ()
bot n inc outc = handle prefix
    where handle msg = do putStrLn "waiting on input from server..."
                          q <- readInput inc
                          putStrLn "got input from server."
                          putStrLn $ show q
                          case q of
                            Info m -> case m of
                                        InfoMessage s -> handle (msg++s++"\n")
                                        -- _ -> putStrLn $ show m
                            Question i m as (a0,a1) ->
                                do putStrLn msg
                                   putStrLn $ "Question: " ++ show m
                                   putStrLn "Options:"
                                   let pretty (Choose s) = s
                                       pretty (PickCard c) = cname c++
                                           " ("++show (cprice c)++")"
                                   mapM_ (\(n,a) -> putStrLn $ "  " ++ show n
                                                    ++ ": " ++ pretty a) $
                                                    zip [1..] as
                                   putStr $ "Enter " ++ show a0 ++ " to "
                                              ++ show a1 ++
                                              " numbers, separated by spaces: "
                                   hFlush stdout
                                   numpicked <- randomRIO (a0,a1)
                                   let r = take numpicked as
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

server :: Output (Message String MessageToClient)
       -> Input (Message String (ServerMessage ResponseFromClient))
       -> IO ()
server o i = do Message player1 _ N <- readInput i
                Message player2 _ N <- readInput i
                (iplayer1, oplayer1) <- pipe
                (iplayer2, oplayer2) <- pipe
                forkIO $ forever $ readInput iplayer1 >>=
                                      (writeOutput o . Message "server" player1)
                forkIO $ forever $ readInput iplayer2 >>=
                                      (writeOutput o . Message "server" player2)
                (i2s, o2s) <- pipe
                forkIO $ forever $ do Message _ _ (M m) <- readInput i
                                      writeOutput o2s m
                state <- start [(player1,oplayer1),(player2,oplayer2)] i2s
                         [secretChamber,chapel,cellar,village,remodel,
                          smithy,militia,caravan,mine,market]
                evalStateT play state >>= print

main :: IO ()
main =
    do args <- getArgs
       case args of
         [name@('b':'o':'t':_),hostname] ->
             runClientTCP hostname 12345 $ ioClient $ bot name
         [name,hostname] ->
             runClientTCP hostname 12345 $ ioClient $ client name
         ["server"] ->
             runServerTCP 12345 $ ioServer server
         _ -> do
          (c1i, c1o) <- pipe
          (c2i, c2o) <- pipe
          (cout_i, cout_o) <- pipe
          forkIO $ client "Alice" c1i cout_o
          forkIO $ client "Bob" c2i cout_o
          state <- start [("Alice",c1o),("Bob",c2o)] cout_i
                   [secretChamber,chapel,cellar,village,remodel,
                    smithy,militia,caravan,mine,market]
          evalStateT play state >>= print
