||| Merge spurious conflicts. >>>

<<< Merge spurious conflicts. |||
||| resolve conflicts >>>
{-# LANGUAGE PatternGuards #-}


<<< resolve conflicts |||
import Dominion

import TCP.Message ( Message(..) )
import TCP.Client ( runClientTCP, ioClient )
import TCP.Server ( runServerTCP )
import TCP.ServerTypes ( ServerMessage(..), modifyServer, ioServer )
import NamePicker ( simpleNamedClient, pickNames )

import Control.Concurrent ( forkIO )
import TCP.Chan ( Input, Output, readInput, writeOutput, pipe )
import Control.Monad ( forever, when, replicateM, forM_ )
import Control.Monad.State ( evalStateT, execStateT,
                             StateT, runStateT, put, get, modify, liftIO )
import Data.Char ( toLower, isSpace )
import Data.List ( sortBy, (\\) )
import Data.Ord ( comparing )
import Data.IORef ( newIORef, readIORef, writeIORef )
import System.IO ( hFlush, stdout )
import System.Environment ( getArgs )
import System.Random ( randomRIO )

data PlayerFunctions = PlayerFunctions {
      serverStatus   :: String -> IO PlayerFunctions,
      receiveInfo    :: InfoMessage -> IO PlayerFunctions,
      answerQuestion :: QuestionMessage -> [Answer] -> (Int,Int)
                     -> IO ([Answer],PlayerFunctions)
    }

client :: PlayerFunctions -> String
       -> Input MessageToClient -> Output ResponseFromClient -> IO ()
client p n inc outc = handle p
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
    where status = liftIO . putStrLn
          info m = case m of
                     InfoMessage s -> modify (++(s++"\n"))
                     --
          answer :: QuestionMessage -> [Answer] -> (Int,Int) -> StateT String IO [Answer]
          answer m as (a0,a1) =
              do get >>= liftIO . putStrLn
                 put ""
                 liftIO $ putStrLn $ "Question: " ++ show m
                 liftIO $ putStrLn "Options:"
                 let pretty (Choose s) = s
                     pretty (PickCard c) = cname c++" ("++show (cprice c)++") ["++
                                           show (cid c)++"]"
                 forM_ (zip [1..] as) $
                   \(n,a) -> liftIO $ putStrLn $ "  " ++ show n ++ ": " ++ pretty a
                 ans <- untilJust $ do
                   liftIO $ putStr $ "Enter " ++ show a0 ++ " to " ++ show a1 ++
                                     " numbers, separated by spaces: "
                   liftIO $ hFlush stdout
                   (ints (length as) . words) `fmap` liftIO getLine
                 return $ map (\n -> as!!(n-1)) ans
          spaces = replicate ((40-length name)`div`2) ' '
          ints n [] = Just []
          ints n (s:ss) = case readsPrec 0 s of
                            [(x,"")] | x <= n && x > 0 -> (x:) `fmap` ints n ss
                                     | otherwise -> Nothing
                            _ -> Nothing
          untilJust job = do ma <- job
                             case ma of
                               Just a -> return a
                               Nothing -> untilJust job
          prefix = unlines["",replicate 40 '=',
                           spaces++name++spaces,
                           replicate 40 '-',""]

weights :: QuestionMessage -> Answer -> Double
weights (TrashBecause _) (PickCard c)
        | cname c == "Curse" = 1000
        | cname c == "Estate" = 20
        | cname c == "Copper" = 2
        | otherwise = 0
weights (DiscardBecause _) (PickCard c)
        | cname c == "Curse" = 100
        | cname c == "Estate" = 100
        | cname c == "Duchy" = 100
        | cname c == "Province" = 100
        | cname c == "Copper" = 3
        | cname c == "Gold" = 0
        | cname c == "Village" = 0
        | cname c == "Bazaar" = 0
        | cname c == "Market" = 0
        | otherwise = 0
weights _ (PickCard c) | cname c == "Cellar" = 0
                       | cname c == "Province" = 100
                       | cname c == "Gold" = 40
                       | cname c == "Silver" = 10
                       | cname c == "Warehouse" = 0
                       | cname c == "Copper" = 0
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
wmoney (TrashBecause _) (PickCard c)
    | cname c == "Curse" = 1000
    | cname c == "Estate" = 20
    | cname c == "Duchy" = 5
    | cname c == "Copper" = 2
    | cname c == "Silver" = 0.01
    | otherwise = 0
wmoney (DiscardBecause _) (PickCard c) | cname c == "Curse" = 100
                                       | cname c == "Estate" = 100
                                       | cname c == "Duchy" = 100
                                       | cname c == "Province" = 100
                                       | cname c == "Copper" = 3
                                       | cname c == "Silver" = 0.5
                                       | otherwise = 0
wmoney _ (PickCard c) | cname c == "Province" = 1000
                      | cname c == "Harem" = 80
                      | cname c == "Gold" = 40
                      | cname c == "Silver" = 2
                      | cname c == "Duchy" = 0.1
wmoney _ _ = 0

weightedBot :: (QuestionMessage -> Answer -> Double) -> PlayerFunctions
weightedBot weight = ioToPlayer status info answer
    where status _ = return ()
          info _   = return ()
          pick wtot was = seek was `fmap` randomRIO (0,wtot)
          seek [(_,a)] _ = a
          seek ((w,a):was) p | p <= w = a
                             | otherwise = seek was (p-w)
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
          pretty (Choose s) = s
          pretty (PickCard c) = cname c++" ("++show (cprice c)++")"


randomBot :: PlayerFunctions
randomBot = ioToPlayer status info answer
    where status _ = return ()
          info _   = return ()
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
          lookupBy f s [] = Nothing
          lookupBy f s (a:as) | lc (f a) == lc s = Just a
                              | otherwise        = lookupBy f s as

pickDecks :: [Card] -> IO [Card]
pickDecks cs = do let ||| add recommended deck sets. >>>
all = dominion ++ promos ++ intrigue ++ seaside
                   sets = filter ((==10).length.snd)
                          (dominionSets++intrigueSets)
               r <- randomRIO (1,100)
               
<<< add recommended deck sets. |||
||| resolve conflicts >>>
sets = filter ((==10).length.snd) allRecommended
                  r <- randomRIO (1,100)
                  
<<< resolve conflicts |||
decks <- ||| add recommended deck sets. >>>
if r > (10 :: Int)
                        then 
<<< add recommended deck sets. |||
||| resolve conflicts >>>
if r > (10 :: Int) || not (null cs)
                           then (
<<< resolve conflicts |||
take 10 . (cs++)) `fmap` shuffleIO ||| add recommended deck sets. >>>
all
                        else do (sn,d) <- head `fmap` shuffleIO sets
                                putStrLn ("Using set "++sn)
                                return d

<<< add recommended deck sets. |||
||| resolve conflicts >>>
allDecks
                           else do (sn,d) <- head `fmap` shuffleIO sets
                                   putStrLn ("Using set "++sn)
                                   return d
   
<<< resolve conflicts |||
               return $ sortBy (comparing cardPrice) decks

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
