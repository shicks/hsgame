{-# LANGUAGE PatternGuards #-}

module HTTP.Dominion ( dominionHandler, MessageToGame(..), game ) where

import Dominion ( Card, MessageToClient, ResponseFromClient, play, evalGame,
                  shuffleIO, allRecommended, allDecks )
import qualified Dominion ( start )
import TCP.Chan ( Output, pipe, readInput )

import HTTP.Response ( Response, jsPrintf, blank200, error404 )
import HTTP.LoginServer ( Agent )
import HTTP.Handlers ( Handler(..), Message(..) )

import System.Random ( randomRIO )
import Data.Maybe ( fromMaybe )
import Control.Monad ( forM_, forever )
import Control.Concurrent ( forkIO )

data MessageToGame = StartGame
                   | NewPlayer Agent (Output MessageToClient)

-- The javascript client can tell US how it wants us to respond, i.e.
-- where to store the answer...
dominionHandler :: String -> (Agent -> String -> IO ()) -> IO Handler
dominionHandler area sendmess =
    do (i,o) <- pipe -- ResponseFromClient
       let handle a ps q =
               Message $ \st ->
                   do putStrLn $ "dominionHandler <o> "++show a++" "++
                               show ps++" " ++show q
                      handler area sendmess st a ps q
           startg ds = do decks <- pickDecks []
                          cls <- mapM (manageClient sendmess ds) (clients ds)
                          state <- Dominion.start cls i decks
                          forkIO (evalGame play state >>= print)
                          return ()
           initstate = DomState [] o startg
       return $ Handler initstate handle

manageClient :: (Agent -> String -> IO ()) -> DomState -> (Agent, a)
             -> IO (String, Output MessageToClient)
manageClient sendmess ds (a,_) =
    do (i,o) <- pipe
       forkIO $ forever $ do x <- readInput i
                             putStrLn ("got message "++show x++" for "++show a)
                             -- FIXME: no privacy here!
                             say sendmess ds ("for "++show a++": "++show x)
       return (show a, o)

data DomState = DomState { clients :: [(Agent, String -> String)],
                           game :: Output ResponseFromClient,
                           start :: DomState -> IO () }

handler :: String -> (Agent -> String -> IO ())
        -> DomState
        -> Agent -> [String] -> [(String,String)]
        -> IO (DomState, Response)
handler _ sendmess ds _ ["start"] _ =
    do putStrLn "game should start now..."
       say sendmess ds "Game is starting! (just kidding)"
       say sendmess ds ("Players: "++unwords (map (show . fst) $ clients ds))
       start ds ds
       r <- blank200
       return (ds, r)
handler area sendmess ds a ["join"] q =
    do let clients' = (a,f) : filter ((/= a) . fst) (clients ds)
           ds' = ds { clients = clients' }
       say sendmess ds' ("Welcome "++show a)
       r <- blank200
       return (ds', r)
    where f s = jsPrintf (fromMaybe js $ lookup "q" q) [s]
          js = "$."++area++".say(%s+\"\\n\")"
handler _ sendmess ds a ["leave"] _ =
    do let cl' = filter ((/= a) . fst) $ clients ds
           ds' = ds { clients = cl' }
       say sendmess ds' ("Goodbye "++show a)
       r <- blank200
       return (ds', r)
handler _ sendmess ags a ["say"] q =
    do let msg = fromMaybe "(noinput)" $ lookup "q" q
       say sendmess ags (show a++": "++msg)
       r <- blank200
       return (ags, r)
handler _ _ ags _ _ _ =
    do putStrLn "handler _ _ _ _"
       r <- error404
       return (ags, r)

say :: (Agent -> String -> IO ()) -> DomState -> String -> IO ()
say sendmess ags s =
    forM_ (clients ags) $ \(a,f) -> sendmess a (f s)

pickDecks :: [Card] -> IO [Card]
pickDecks cs = do let sets = filter ((==10).length.snd) allRecommended
                  r <- randomRIO (1,100)
                  decks <- if r > (10 :: Int) || not (null cs)
                           then (take 10 . (cs++)) `fmap` shuffleIO allDecks
                           else do (sn,d) <- head `fmap` shuffleIO sets
                                   putStrLn ("Using set "++sn)
                                   return d
                  return decks
