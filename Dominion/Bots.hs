module Dominion.Bots ( pickDecks, PlayerFunctions(..), client,
                       dumbBot, moneyBot, strategyBot, sillyBot, randomBot )
    where

import Dominion.Types ( MessageToClient(..), ResponseFromClient(..),
                        QuestionMessage(..), InfoMessage(..), Answer(..),
                        Card, cardPrice, cname )
import Dominion.Cards ( allRecommended, allDecks )
import Dominion.Stack ( shuffleIO )
import Dominion.Pretty ( pretty )

import TCP.Chan ( Input, Output, readInput, writeOutput )

import System.Random ( randomRIO )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( replicateM )
import Data.List ( sortBy )
import Data.Ord ( comparing )

strategyBot :: String -> Input MessageToClient -> Output ResponseFromClient
            -> IO ()
strategyBot = client $ strategyBot0 wmoney

moneyBot :: String -> Input MessageToClient -> Output ResponseFromClient
         -> IO ()
moneyBot = client $ weightedBot wmoney

sillyBot :: String -> Input MessageToClient -> Output ResponseFromClient
         -> IO ()
sillyBot = client $ weightedBot weights

dumbBot :: String -> Input MessageToClient -> Output ResponseFromClient -> IO ()
dumbBot = client randomBot

pickDecks :: [Card] -> IO [Card]
pickDecks cs = do let sets = filter ((==10).length.snd) allRecommended
                  r <- randomRIO (1,100)
                  decks <- if r > (10 :: Int) || not (null cs)
                           then (take 10 . (cs++)) `fmap` shuffleIO allDecks
                           else do (sn,d) <- head `fmap` shuffleIO sets
                                   putStrLn ("Using set "++sn)
                                   return d
                  return $ sortBy (comparing cardPrice) decks


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

strategyBot0 :: (QuestionMessage -> Answer -> Double) -> PlayerFunctions
strategyBot0 weight = PlayerFunctions status info answer
    where status x = putStrLn x >> return (strategyBot0 weight)
          info (GameOver m) = putStrLn m >> exitWith ExitSuccess
          info x = putStrLn (show x) >> return (strategyBot0 weight)
          answer SelectBuys as _
              | not $ null $ filter isfun as =
                  return (filter isfun as, weightedBot weight)
          answer q as m =
              do (a,_) <- (answerQuestion (weightedBot weight)) q as m
                 return (a, strategyBot0 weight)
          isfun (PickCard c) = cname c `elem` ["Chapel", "Moneylender"]
          isfun _ = False

weightedBot :: (QuestionMessage -> Answer -> Double) -> PlayerFunctions
weightedBot weight = ioToPlayer status info answer
    where status _ = return ()
          info (GameOver m) = putStrLn m >> exitWith ExitSuccess
          info _   = return ()
          pick wtot was = seek was `fmap` randomRIO (0,wtot)
          seek [(_,a)] _ = a
          seek ((w,a):was) p | p <= w = a
                             | otherwise = seek was (p-w)
          seek [] _ = error "seek in weightedBot called with empty list"
          answer _ _ (0,0) = return []
          answer q as (a,b) =
              do let ws = zipWith weight (repeat q) as
                     wtot = sum ws
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
          info (GameOver m) = putStrLn m >> exitWith ExitSuccess
          info _ = return ()
          answer _ as (a,b) = do n <- randomRIO (a,b)
                                 take n `fmap` shuffleIO as
