{-# LANGUAGE PatternGuards, FlexibleInstances #-}

import Dominion

import TCP.Message ( Message(..) )
import TCP.Client ( runClientTCP, ioClient )
import TCP.Server ( runServerTCP )
import TCP.ServerTypes ( ServerMessage(..), modifyServer, ioServer )
import NamePicker ( simpleNamedClient, pickNames )

import Control.Concurrent ( forkIO )
import TCP.Chan ( Input, Output, readInput, writeOutput, pipe )
import Control.Monad ( forever )
import Control.Monad.State ( execStateT,
                             StateT, runStateT, put, get, modify, liftIO )
import Data.Char ( toLower, isSpace )
import System.IO ( hFlush, stdout )
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..) )

stateToPlayer :: (String -> StateT s IO ())
              -> (InfoMessage -> StateT s IO ())
              -> (QuestionMessage -> [Answer] -> (Int,Int) -> StateT s IO [Answer])
              -> s -> PlayerFunctions
stateToPlayer ss ri aq s0 = PlayerFunctions
                            (\s -> this `fmap` execStateT (ss s) s0)
                            (\m -> this `fmap` execStateT (ri m) s0)
                            (\q as r -> (id *** this)`fmap`runStateT (aq q as r) s0)
    where this = stateToPlayer ss ri aq
          (***) f g (a,b) = (f a,g b)

stdioClient :: String -> Input MessageToClient -> Output ResponseFromClient -> IO ()
stdioClient name = client (stateToPlayer status info answer "") name
    where status _ = return () -- status = liftIO . hPutStrLn stderr
          info (GameOver m) = liftIO $ putStrLn m >> exitWith ExitSuccess
          info m = modify (++(pretty m++"\n"))
          answer :: QuestionMessage -> [Answer] -> (Int,Int) -> StateT String IO [Answer]
          answer m as (a0,a1) =
              do get >>= liftIO . putStrLn
                 put ""
                 liftIO $ putStrLn $ pretty $ Question 0 m as (a0,a1)
                 ans <- untilJust $ do
                   liftIO $ putStr $ "Enter " ++ showRange a0 a1
                              ++ " separated by spaces: "
                   liftIO $ hFlush stdout
                   (ints (length as) . words) `fmap` liftIO getLine
                 return $ map (\n -> as!!(n-1)) ans
          ints _ [] = Just []
          ints n (s:ss) = case readsPrec 0 s of
                            [(x,"")] | x <= n && x > 0 -> (x:) `fmap` ints n ss
                                     | otherwise -> Nothing
                            _ -> Nothing
          untilJust job = do ma <- job
                             case ma of
                               Just a -> return a
                               Nothing -> untilJust job
          showRange 0 1 = "up to 1 number"
          showRange 0 a = "up to " ++ show a ++ " numbers"
          showRange 1 1 = "1 number"
          showRange a b | a==b = show a ++ " numbers"
                        | b==a+1 = show a ++ " or " ++ show b ++ " numbers"
                        | otherwise = " from " ++ show a ++ " to " ++ show a
                                      ++ " numbers"

server :: [Card] -> Output (Message String MessageToClient)
       -> Input (Message String (ServerMessage ResponseFromClient))
       -> IO ()
server cs o i = do Message player1 _ N <- readInput i
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
                   decks <- pickDecks cs
                   state <- start [(player1,oplayer1),(player2,oplayer2)]
                            i2s decks
                   evalGame play state >>= print

main :: IO ()
main = getArgs >>= mainArgs []

mainArgs :: [Card] -> [String] -> IO ()
mainArgs cs as
    = case as of
        [name@('s':'t':'r':_),hostname] ->
             runClientTCP hostname 12345 $ simpleNamedClient name $ ioClient $
                          strategyBot name
        [name@('m':'o':'n':_),hostname] ->
             runClientTCP hostname 12345 $ simpleNamedClient name $ ioClient $
                          moneyBot name
        [name@('b':'o':'t':'2':_),hostname] ->
             runClientTCP hostname 12345 $ simpleNamedClient name $ ioClient $
                          sillyBot name
        a:as' | Just c <- lookupBy cardName a allDecks -> mainArgs (c:cs) as'
              | Just d <- lookupBy fst a allRecommended ->
                                                  mainArgs (cs++snd d) as'
        [name@('b':'o':'t':_),hostname] ->
            runClientTCP hostname 12345 $ simpleNamedClient name $ ioClient $
                          dumbBot name
        [name,hostname] ->
            runClientTCP hostname 12345 $ simpleNamedClient name $ ioClient $
                          stdioClient name
        ["server"] ->
            runServerTCP 12345 $ modifyServer (pickNames "client" "server") $
                          ioServer (server cs)
        _ -> twoPlayer cs
    where lc = filter (not . isSpace) . map toLower
          lookupBy :: (a -> String) -> String -> [a] -> Maybe a
          lookupBy _ _ [] = Nothing
          lookupBy f s (x:xs) | lc (f x) == lc s = Just x
                              | otherwise        = lookupBy f s xs

twoPlayer :: [Card] -> IO ()
twoPlayer cs = do
          (c1i, c1o) <- pipe
          (c2i, c2o) <- pipe
          (cout_i, cout_o) <- pipe
          forkIO $ stdioClient "Alice" c1i cout_o
          forkIO $ stdioClient "Bob" c2i cout_o
          decks <- pickDecks cs
          state <- start [("Alice",c1o),("Bob",c2o)] cout_i decks
          evalGame play state >>= print

-- Crazy ideas

-- data MonadRunner m = MonadRunner (forall a. m a -> IO (a,MonadRunner m))
-- stateMonadRunner :: s -> MonadRunner (StateT s IO)
-- stateMonadRunner s0 = MonadRunner $ \ma -> runStateT >>= second stateMonadRunner
-- monadToPlayer :: Monad m
--               => (String -> m ()) -> (InfoMessage -> m ())
--               -> (QuestionMessage -> [Answer] -> (Int,Int) -> m [Answer])
--               -> MonadRunner m -> PlayerFunctions
-- ioMonadRunner :: MonadRunner IO
-- ioMonadRunner = MonadRunner $ fmap $ flip (,) ioMonadRunner

-- class StatePlayer s p | p -> s where
--     spServerStatus   :: p -> String -> StateT s IO ()
--     spReceiveInfo    :: p -> InfoMessage -> StateT s IO ()
--     spAnswerQuestion :: p -> QuestionMessage -> [Answer] -> (Int,Int)
--                      -> StateT s IO [Answer]
-- instance StatePlayer String stdioPlayer where
