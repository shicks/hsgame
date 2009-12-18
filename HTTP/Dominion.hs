{-# LANGUAGE PatternGuards #-}

module HTTP.Dominion ( dominionHandler, MessageToGame(..) ) where

import Dominion ( MessageToClient(Question, Info), QId, Answer,
                  ResponseFromClient(ResponseFromClient), shuffleIO,
                  play, evalGame, pretty, pickDecks, strategyBot )
import qualified Dominion ( start )
import TCP.Chan ( Output, pipe, readInput, writeOutput )

import HTTP.Response ( Response, jsPrintf, blank200, error404 )
import HTTP.LoginServer ( Agent )
import HTTP.Handlers ( Handler(..), Message(..) )

import Data.Maybe ( fromMaybe )
import Control.Monad ( forever )
import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, takeMVar )

data MessageToGame = StartGame
                   | NewPlayer Agent (Output MessageToClient)

-- The javascript client can tell US how it wants us to respond, i.e.
-- where to store the answer...
dominionHandler :: String -> (Agent -> String -> IO ()) -> IO Handler
dominionHandler area0 sendmess =
    do (i,o) <- pipe -- ResponseFromClient
       latestq <- newEmptyMVar
       let handle a ps q =
               Message $ \st ->
                   do putStrLn $ "dominionHandler <o> "++show a++" "++
                               show ps++" " ++show q
                      handler sendmess st a ps q
           startg ds = do decks <- pickDecks []
                          cls <- mapM (manageClient sendmess ds) (clients ds)
                          bts <- mapM newbot (bots ds)
                          state <- Dominion.start (cls++bts) i decks
                          forkIO (evalGame play state >>= print)
                          return ()
           newbot bname = do (ib,ob) <- pipe
                             forkIO $ strategyBot bname ib o
                             return (bname, ob)
           initstate = DomState area0 [] [] o latestq startg
       return $ Handler initstate handle

manageClient :: (Agent -> String -> IO ()) -> DomState -> Agent
             -> IO (String, Output MessageToClient)
manageClient sendmess ds a =
    do (i,o) <- pipe
       forkIO $ forever $
              do x <- readInput i
                 putStrLn ("got message for "++show a++":\n"++pretty x)
                 mapM_ (sayto sendmess ds a) (lines $ pretty x)
                 case x of
                   Question qid _ as mnmx ->
                       do putStrLn "got question..."
                          putMVar (latestQ ds) (qid, as, mnmx)
                   Info _ -> putStrLn "got info."
       return (show a, o)

data DomState = DomState { area :: String,
                           clients :: [Agent],
                           bots :: [String],
                           game :: Output ResponseFromClient,
                           latestQ :: MVar (QId, [Answer], (Int,Int)),
                           start :: DomState -> IO () }

handler :: (Agent -> String -> IO ())
        -> DomState
        -> Agent -> [String] -> [(String,String)]
        -> IO (DomState, Response)
handler sendmess ds _ ["start"] _ =
    do putStrLn "game should start now..."
       say sendmess ds "Game is starting!"
       say sendmess ds ("Players: "++unwords (map show $ clients ds))
       say sendmess ds ("Bots: "++unwords (map show $ bots ds))
       start ds ds
       r <- blank200
       return (ds, r)
handler sendmess ds a ["join"] _ =
    do let ds' = ds { clients = a : filter (/= a) (clients ds) }
       say sendmess ds' ("Welcome "++show a)
       r <- blank200
       return (ds', r)
handler sendmess ds a ["leave"] _ =
    do let cl' = filter (/= a) $ clients ds
           ds' = ds { clients = cl' }
       say sendmess ds' ("Goodbye "++show a)
       r <- blank200
       return (ds', r)
handler sendmess ags a ["say"] q =
    do let msg = fromMaybe "(noinput)" $ lookup "q" q
       say sendmess ags (show a++": "++msg)
       r <- blank200
       return (ags, r)
handler sendmess ds _ ["addbot"] _ =
    do r <- blank200
       let botnames = ["Hadaly", "Olympia", "Tik-Tok", "Zat", "Rex",
                       "Helen O'Loy", "Adam Link", "Gnut", "Jay Score",
                       "Robbie", "Speedie", "Cutie", "L-76", "Z-1", "Z-2",
                       "Z-3", "Emma-2", "Brackenridge", "Tony", "Lenny",
                       "Ez-27", "R. Daneel Olivaw", "R. Giskard Reventlov",
                       "Andrew Martin", "Norby", "Bors", "Zane Gort", "SHROUD",
                       "SHOCK", "Frost", "Trurl", "Klapaucius",
                       "Marvin the Paranoid Android", "Dorfl", "Muffit II",
                       "T-800", "ED-209", "Dot Matrix", "T-1000", "T-850",
                       "Data", "HAL", "R2-D2", "C-3PO"]
       n <- (head . filter (`notElem` bots ds)) `fmap` shuffleIO botnames
       say sendmess ds ("Adding a bot named "++n)
       return (ds { bots = n : bots ds}, r)
handler sendmess ds a ["answer"] q =
 do putStrLn "checking on the question... (should be maybe version)"
    (qid, as, (mn,mx)) <- takeMVar $ latestQ ds
    case lookup "q" q of
      Just ns ->
          do let toa x = case reads x of
                           [(num,"")] | num < 1 -> []
                                      | num > length as -> []
                                      | otherwise -> [as !! (num-1)]
                           _ -> []
                 myas = concatMap toa $ words ns
             if length myas > mx || length myas < mn
                then sayto sendmess ds a "Bad answer!"
                else writeOutput (game ds) $ ResponseFromClient qid myas
      _ -> putStrLn "This doesn't seem quite right..."
    r <- blank200
    return (ds, r)
handler _ ags _ _ _ =
    do putStrLn "handler _ _ _ _"
       r <- error404
       return (ags, r)

sayto :: (Agent -> String -> IO ()) -> DomState -> Agent -> String -> IO ()
sayto sendmess ds a s = sendmess a $ jsPrintf ("$."++area ds++".say(%s)") [s]

say :: (Agent -> String -> IO ()) -> DomState -> String -> IO ()
say sendmess ds s = mapM_ (\a -> sendmess a mess) (clients ds)
    where mess = jsPrintf ("$."++area ds++".say(%s)") [s]
