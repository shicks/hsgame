{-# LANGUAGE PatternGuards, FlexibleInstances #-}

import Dominion

import TCP.Message ( Message(..) )
import TCP.Client ( runClientTCP, ioClient )
import TCP.Server ( runServerTCP )
import TCP.ServerTypes ( ServerMessage(..), modifyServer, ioServer )
import NamePicker ( simpleNamedClient, pickNames )

import Control.Concurrent ( forkIO )
import TCP.Chan ( Input, Output, readInput, writeOutput, pipe )
import Control.Monad ( forever, replicateM )
import Control.Monad.State ( execStateT,
                             StateT, runStateT, put, get, modify, liftIO )
import Data.Char ( toLower, isSpace )
import System.IO ( hFlush, stdout )
import System.Environment ( getArgs )
import System.Random ( randomRIO )
import System.Exit ( exitWith, ExitCode(..) )

data PlayerFunctions = PlayerFunctions {
      serverStatus   :: String -> IO PlayerFunctions,
      receiveInfo    :: InfoMessage -> IO PlayerFunctions,
      answerQuestion :: QuestionMessage -> [Answer] -> (Int,Int)
                     -> IO ([Answer],PlayerFunctions)
    }

client :: PlayerFunctions -> String
       -> Input MessageToClient -> Output ResponseFromClient -> IO ()
client p _ inc outc = handle p
    where handle p0 =
              do p1 <- serverStatus p0 "waiting on input from server..."
                 q <- readInput inc
                 p3 <- serverStatus p1 "got input from server."
                 -- p3 <- serverStatus p2 $ show q
                 case q of
                   Info m -> receiveInfo p3 m >>= handle
                   Question i m as (a0,a1) ->
                       do (rs,p4) <- answerQuestion p3 m as (a0,a1)
                          writeOutput outc $ ResponseFromClient i rs
                          handle p4

ioToPlayer :: (String -> IO ()) -> (InfoMessage -> IO ())
           -> (QuestionMessage -> [Answer] -> (Int,Int) -> IO [Answer])
           -> PlayerFunctions
ioToPlayer ss ri aq = PlayerFunctions
                      (\s -> ss s >> return this)
                      (\m -> ri m >> return this)
                      (\q as r -> flip (,) this `fmap` aq q as r)
    where this = ioToPlayer ss ri aq

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

wantBad :: QuestionMessage -> Bool
wantBad (GiveAway _) = True
wantBad (TrashBecause _) = True
wantBad _ = False

wantUseless :: QuestionMessage -> Bool
wantUseless (DiscardBecause _) = True
wantUseless _ = False

weights :: QuestionMessage -> Answer -> Double
weights q (PickCard c)
        | wantBad q && cname c == "Curse" = 1000
        | wantBad q && cname c == "Estate" = 20
        | wantBad q && cname c == "Copper" = 2
        | wantBad q = 0
        | wantUseless q && cname c == "Curse" = 100
        | wantUseless q && cname c == "Estate" = 100
        | wantUseless q && cname c == "Duchy" = 100
        | wantUseless q && cname c == "Province" = 100
        | wantUseless q && cname c == "Copper" = 3
        | wantUseless q = 0
        | cname c == "Cellar" = 0
        | cname c == "Province" = 100
        | cname c == "Gold" = 40
        | cname c == "Silver" = 10
        | cname c == "Warehouse" = 0
        | cname c == "Copper" = 1e-9 -- this just needs to beat curse...
        | cname c == "Curse" = 0
        | cname c == "Estate" = 0
        | cname c == "Chancellor" = 0
        | cname c == "Bureaucrat" = 0
        | cname c == "Chapel" = 0.1
        | cname c == "Council Room" = 0.3
        | cname c == "Feast" = 1.5
        | cname c == "Festival" = 7
        | cname c == "Laboratory" = 7
        | cname c == "Market" = 7
        | cname c == "Militia" = 2
        | cname c == "Mine" = 2
        | cname c == "Village" = 6
        | cname c == "Smithy" = 7
        | cname c == "Spy" = 0
        | cname c == "Courtyard" = 0
        | cname c == "Thief" = 2
weights _ _ = 1

wmoney :: QuestionMessage -> Answer -> Double
wmoney SelectAction (PickCard c) | cname c == "Chapel" = 1000
                                 | cname c == "Moneylender" = 1000
                                 | cname c == "Village" = 10000
                                 | cname c == "Market" = 10000
                                 | cname c == "Pearl Diver" = 10000
                                 | cname c == "Great Hall" = 10000
                                 | cname c == "Smithy" = 100
wmoney q (PickCard c)
    | wantBad q && cname c == "Curse" = 1000
    | wantBad q && cname c == "Estate" = 20
    | wantBad q && cname c == "Duchy" = 5
    | wantBad q && cname c == "Copper" = 2
    | wantBad q && cname c == "Silver" = 0.01
    | wantBad q = 0
    | wantUseless q && cname c == "Curse" = 100
    | wantUseless q && cname c == "Estate" = 100
    | wantUseless q && cname c == "Duchy" = 100
    | wantUseless q && cname c == "Province" = 100
    | wantUseless q && cname c == "Copper" = 3
    | wantUseless q && cname c == "Silver" = 0.5
    | wantUseless q = 0
    | cname c == "Province" = 1000
    | cname c == "Harem" = 80
    | cname c == "Gold" = 40
    | cname c == "Silver" = 2
    | cname c == "Duchy" = 0.1
wmoney _ _ = 0

strategyBot :: (QuestionMessage -> Answer -> Double) -> PlayerFunctions
strategyBot weight = PlayerFunctions status info answer
    where status x = putStrLn x >> return (strategyBot weight)
          info (GameOver m) = putStrLn m >> exitWith ExitSuccess
          info x = putStrLn (show x) >> return (strategyBot weight)
          answer SelectBuys as _
              | not $ null $ filter isfun as =
                  return (filter isfun as, weightedBot weight)
          answer q as m =
              do (a,_) <- (answerQuestion (weightedBot weight)) q as m
                 return (a, strategyBot weight)
          isfun (PickCard c) = cname c `elem` ["Chapel", "Moneylender"]
          isfun _ = False

weightedBot :: (QuestionMessage -> Answer -> Double) -> PlayerFunctions
weightedBot weight = ioToPlayer status info answer
    where status _ = return ()
          info (GameOver m) = liftIO $ putStrLn m >> exitWith ExitSuccess
          info _   = return ()
          pick wtot was = seek was `fmap` randomRIO (0,wtot)
          seek [(_,a)] _ = a
          seek ((w,a):was) p | p <= w = a
                             | otherwise = seek was (p-w)
          seek [] _ = error "seek in weightedBot called with empty list"
          answer _ _ (0,0) = return []
          answer q as (a,b) =
              liftIO $ do let ws = zipWith weight (repeat q) as
                          let wtot = sum ws
                          n0 <- randomRIO (0.5,wtot)
                          let n = max a $ min (round n0) b
                          putStrLn ("deciding between: "++show n)
                          x <- replicateM n $ pick wtot (zip ws as)
                          putStrLn "Chose:"
                          mapM_ (\aa -> putStrLn (show q++" "++pretty aa)) x
                          return x


randomBot :: PlayerFunctions
randomBot = ioToPlayer status info answer
    where status _ = return ()
          info (GameOver m) = liftIO $ putStrLn m >> exitWith ExitSuccess
          info _ = return ()
          answer _ as (a,b) = liftIO $ do n <- randomRIO (a,b)
                                          take n `fmap` shuffleIO as

botClient :: String -> Input MessageToClient -> Output ResponseFromClient -> IO ()
botClient = client randomBot
                                 
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
                          client (strategyBot wmoney) name
        [name@('m':'o':'n':_),hostname] ->
             runClientTCP hostname 12345 $ simpleNamedClient name $ ioClient $
                          client (weightedBot wmoney) name
        [name@('b':'o':'t':'2':_),hostname] ->
             runClientTCP hostname 12345 $ simpleNamedClient name $ ioClient $
                          client (weightedBot weights) name
        a:as' | Just c <- lookupBy cardName a allDecks -> mainArgs (c:cs) as'
              | Just d <- lookupBy fst a allRecommended ->
                                                  mainArgs (cs++snd d) as'
        [name@('b':'o':'t':_),hostname] ->
            runClientTCP hostname 12345 $ simpleNamedClient name $ ioClient $
                          botClient name
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
